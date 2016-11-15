/*
 * Copyright 2016 org.NLP4L
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.nlp4l.framework.builtin.spark.mllib

import java.io.{File, _}

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import org.nlp4l.framework.builtin.lucene.LuceneIndexingProcessorEmbedded
import org.nlp4l.framework.models.{Record, _}
import org.nlp4l.framework.processors._
import org.nlp4l.lucene.IReader
import org.nlp4l.lucene.stats.TFIDF
import resource._


class LabeledPointProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new LabeledPointProcessor(
      getStrParamRequired("modelDir"),
      getStrParamRequired("labelField"),
      getStrParamRequired("textField"),
      getStrListParam("valuesFields", null),
      getStrParam("idField", null),
      getConfigParamRequired("analyzer"),
      // label index-name mapping file in any, otherwise auto generated from indexed label field
      getStrParam("labelFile", null),
      // feature word file in any, otherwise auto selected from indexed text field
      getStrParam("featuresFile", null),
      // TF-IDF: max document frequency (%)
      getDoubleParam("maxDFPercent", 99) / 100.0,
      // TF-IDF: min document frequency
      getIntParam("minDF", 1),
      // TF-IDF: max features
      getIntParam("maxFeatures", -1),
      // TF-IDF: TF mode
      getStrParam("tfMode", "n"),
      // TF-IDF: smoothing param
      getDoubleParam("smthTerm", 0.4),
      // TF-IDF: IDF mode
      getStrParam("idfMode", "t"),
      // TF-IDF: term boosts file
      getStrParam("termBoostsFile", null)
    )
  }
}

class LabeledPointProcessor(val modelDir: String,
                            val labelField: String,
                            val textField: String,
                            val valuesFields: Seq[String],
                            val idField: String,
                            val analyzer: Config,
                            val labelFileIn: String, // use this if specified
                            val featuresFileIn: String, // use this if specified
                            val maxDFPercent: Double,
                            val minDF: Int,
                            val maxFeatures: Int,
                            val tfMode: String,
                            val smthTerm: Double,
                            val idfMode: String,
                            val termBoostsFile: String
                           )
  extends Processor
    with CommonProcessor {

  val schemaConf = ConfigFactory.parseString(
    s"""
       | schema {
       |   defAnalyzer {
       |     class : org.apache.lucene.analysis.standard.StandardAnalyzer
       |   }
       |   fields = [
       |     {
       |       name : ${labelField}
       |       indexed : true
       |       stored : true
       |     }
       |     {
       |       name : ${textField}
       |       analyzer: ${analyzer.root().render(ConfigRenderOptions.defaults().setOriginComments(false))}
       |       indexed : true
       |       stored : true
       |     }
       |   ]
       | }
        """.stripMargin)

  val settings = new ModelDirSettings(modelDir)

  override def execute(data: Option[Dictionary]): Option[Dictionary] = {

    // create Lucene Index first, and use it.
    val luneceIndexEmbedded = new LuceneIndexingProcessorEmbedded(schemaConf, null, settings.LUCENE_INDEX_DIR, null, true, false)
    luneceIndexEmbedded.write(data.get.recordList)

    val reader = IReader(settings.LUCENE_INDEX_DIR, luneceIndexEmbedded.getSchema())
    try{
      val featuresWords = {
        if (featuresFileIn != null)
          readFeaturesFile(featuresFileIn)
        else
          selectFeatures(reader, textField, minDF, maxDFPercent, maxFeatures)
      }

      val labelMap = {
        if (labelFileIn != null)
          readLabelFile(labelFileIn, settings.LABEL_FILE_SEP)
        else
          selectLabelMap(reader, labelField)
      }

      val docIds = reader.universalset().toList
      val labels = selectFieldValues(reader, docIds, Seq(labelField)).map(m => m(labelField).head).map(labelMap(_)).toVector
      val termBoosts = if (termBoostsFile != null) readTermBoostsFile(termBoostsFile) else Map.empty[String, Double]
      val (features: Seq[String], vectors: Stream[Seq[Double]]) =
          TFIDF.tfIdfVectors(reader, textField, docIds, featuresWords, tfMode, smthTerm, idfMode, termBoosts)
      val idValues: Seq[String] = if (idField != null) data.get.recordList.map(r => r.cellValue(idField).get.toString) else List.empty[String]

      writeLabelFile(labelMap, settings.LABEL_FILE, settings.LABEL_FILE_SEP)

      writeDataFile(labels, vectors, settings.DATA_FILE, settings.DATA_FILE_SEP, idValues)

      writeWordsFile(reader, features, textField, settings.WORDS_FILE, settings.WORDS_FILE_SEP)

      writeTFIDFParamsFile(reader, textField, tfMode, smthTerm, idfMode, termBoostsFile, settings.TFIDF_FILE, settings.TFIDF_FILE_SEP)

      dumpValuesFields(data.get.recordList, valuesFields, settings.VALUES_FILE_DIR)
    }
    finally{
      if (reader != null)
        reader.close
    }

    val result = "success"
    val dict = Dictionary(Seq(
      Record(Seq(
        Cell("result", result)
      ))))
    Some(dict)
  }

  def dumpValuesFields(records: Seq[Record], valuesFields: Seq[String], valuesFileDir: String): Unit = {
    // output additional values
    // a file is created for each field
    if (valuesFields != null) {
      val valuesDir = new File(valuesFileDir)
      if (!valuesDir.exists()) valuesDir.mkdirs()
      valuesFields.foreach(fName => {
        val file = new File(valuesFileDir, "values_" + fName + ".txt")
        for (output <- managed(new PrintWriter(new FileWriter(file)))) {
          records.foreach(r => {
            val line = r.cellValue(fName).get.toString
            output.println(line)
          })
        }
      })
    }
  }
}
