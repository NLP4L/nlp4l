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

import com.typesafe.config.Config
import org.apache.spark.mllib.linalg.Vectors
import org.nlp4l.framework.models.{Record, _}
import org.nlp4l.framework.processors._
import org.nlp4l.lucene._
import org.nlp4l.lucene.stats.{TFIDF, WordCounts}

import scala.collection.mutable.ArrayBuffer

class ClassificationProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new ClassificationProcessor(
      getStrParamRequired("modelDir"),
      getStrParamRequired("textField"),
      getStrParamRequired("idField"),
      getStrListParam("passThruFields", null),
      getConfigParamRequired("analyzer"),
      getStrParam("algorithm", AlgorithmSupport.Default)
    )
  }
}

class ClassificationProcessor(val modelDir: String,
                              val textField: String,
                              val idField: String,
                              val passThruFields: Seq[String],
                              val analyzerConf: Config,
                              val algorithm: String
                             )
  extends Processor
    with CommonProcessor {

  val settings = new ModelDirSettings(modelDir)

  override def execute(dictData: Option[Dictionary]): Option[Dictionary] = {

    val analyzer = SchemaLoader.readAnalyzer(analyzerConf)

    val labelNameMap: Map[Int, String] = readLabelFile(settings.LABEL_FILE, settings.LABEL_FILE_SEP).map(_.swap)

    // wordsMap: term -> (index, document frequency)
    val wordsMap: Map[String, (Int, Long)] = readWordsFile(settings.WORDS_FILE, settings.WORDS_FILE_SEP)
    // Document frequency for each term
    val dfMap: Map[String, Long] = wordsMap.map(m => (m._1, m._2._2))

    val (numDocs, maxTF, tfMode, smthTerm, idfMode, termBoostsFile) = readTFIDFParamsFile(settings.TFIDF_FILE, settings.TFIDF_FILE_SEP)
    val termBoosts = if (termBoostsFile != null) readTermBoostsFile(termBoostsFile) else Map.empty[String, Double]

    val sc = SparkContextLocal.newSparkContext("ClassificationProcessor")
    try {
      // Load model
      val model = algorithm match {
        case AlgorithmSupport.NaiveBayes => {
          NaiveBayesModelSupport.load(sc, settings.MODEL_FILE_DIR)
        }
        case AlgorithmSupport.LogisticRegressionWithLBFGS => {
          LogisticRegressionModelSupport.load(sc, settings.MODEL_FILE_DIR)
        }
        case AlgorithmSupport.DecisionTree => {
          DecisionTreeModelSupport.load(sc, settings.MODEL_FILE_DIR)
        }
        case AlgorithmSupport.RandomForest => {
          RandomForestModelSupport.load(sc, settings.MODEL_FILE_DIR)
        }
        case _ => throw new IllegalArgumentException("unknown algorithm: " + algorithm)
      }
      val resultRecords: ArrayBuffer[Record] = new ArrayBuffer[Record]()
      dictData.get.recordList.foreach {
        record => {
          val text = record.cellMap(textField).value.toString
          val countMap: Map[String, Long] = WordCounts.countWords(text, wordsMap.keySet, analyzer)
          val vectors: Seq[Double] = TFIDF.tfIdfVector(countMap, wordsMap.keySet, tfMode, smthTerm, idfMode, termBoosts, numDocs, maxTF, dfMap)
          val (indices, values) = vectors.zipWithIndex.filter(_._1 != 0).map(t => (t._2, t._1.toDouble)).unzip
          val predict: Double = model.predict(Vectors.sparse(wordsMap.keySet.size, indices.toArray, values.toArray))
          val labelName = labelNameMap(predict.toInt)
          val cells = new ArrayBuffer[Cell]
          // doc id
          cells += record.cellMap(idField)
          // classification
          cells += Cell("classification", labelName)
          // pass thru
          if (passThruFields != null) {
            passThruFields.foreach { passThruField =>
              cells += record.cellMap(passThruField)
            }
          }
          resultRecords += Record(cells.toSeq)
        }
      }
      Some(Dictionary(resultRecords.toSeq))
    }
    finally {
      if (sc != null)
        sc.stop()
    }
  }

}