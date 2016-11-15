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

package org.nlp4l.framework.builtin.kea

import java.io._
import java.nio.file.FileSystems

import com.typesafe.config.Config
import org.apache.commons.io.FileUtils
import org.apache.lucene.document.Document
import org.apache.lucene.index.{DirectoryReader, MultiFields, PostingsEnum, TermsEnum, _}
import org.apache.lucene.search.{DocIdSetIterator, IndexSearcher}
import org.apache.lucene.store.{Directory, FSDirectory}
import org.apache.spark.mllib.feature.MDLPDiscretizer
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.rdd.RDD
import org.joda.time.DateTime
import org.nlp4l.framework.builtin.spark.mllib.SparkContextLocal
import org.nlp4l.framework.models.{Record, _}
import org.nlp4l.framework.processors._
import play.api.Logger
import resource._

import scala.collection.JavaConversions._


class KEAModelBuildProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new KEAModelBuildProcessor(
      getStrParamRequired("textField"),
      getStrParamRequired("keyphrasesField"),
      getStrParam("keyphrasesSep", ","),
      getStrParamRequired("modelDir"),
      getConfigParam("analyzer", null),
      getConfigParam("documentSizeAnalyzer", null),
      getIntParam("minTF", 2)
    )
  }
}

class KEAModelBuildProcessor(val textField: String,
                             val keyphrasesField: String,
                             val keyphrasesSep: String,
                             val modelDir: String,
                             val analyzer: Config,
                             val documentSizeAnalyzer: Config,
                             val minTF: Int
                            )
  extends Processor
    with CommonProcessor {

  val settings = new ModelDirSettings(modelDir)

  private val logger = Logger(this.getClass)

  override def execute(data: Option[Dictionary]): Option[Dictionary] = {

    indexing(settings.LUCENE_INDEX_MODEL_DIR, textField, data, analyzer, documentSizeAnalyzer, withTermVectors = false)

    val keyphrasesMap: Map[String, Set[String]] =
      data.get.recordList.zipWithIndex.map {
        case (record, i) => {
          val keyphrases = record.cellValue(keyphrasesField).get.toString
          val keyphraseSet = keyphrases.split(keyphrasesSep).map(_.trim())
          (i.toString, keyphraseSet.toSet)
        }
      }.map(a => a._1 -> a._2).toMap

    val model = buildModel(keyphrasesMap)
    model.save(settings.FEATURES_MODEL_FILE, settings.KEYPHRASES_MODEL_FILE)

    transformToMDLPDiscretizationModel(settings.FEATURES_MODEL_FILE, settings.CUTP_MODEL_FILE)

    val result = "success"
    val dict = Dictionary(Seq(
      Record(Seq(
        Cell("result", result)
      ))))
    Some(dict)
  }

  def buildModel(knownKeyphrases: Map[String, Set[String]]): KEAModel = {
    val indexDir: Directory = FSDirectory.open(FileSystems.getDefault.getPath(settings.LUCENE_INDEX_MODEL_DIR))
    val ir: IndexReader = DirectoryReader.open(indexDir)
    val model: KEAModel = new KEAModel(ir, knownKeyphrases)
    try {
      (1 to 3).foreach(n => {
        logger.info(new DateTime().toString + " : building " + n + "-gram model")
        val fieldName: String = getFieldName(FIELD_NAME, n)
        val terms: Terms = MultiFields.getTerms(ir, fieldName)
        val te: TermsEnum = terms.iterator
        Iterator.continually(te.next()).takeWhile(_ != null).foreach(rawPhrase => {
          val phrase: String = rawPhrase.utf8ToString
          // use KEAStopFilter instead
          //if(stopWords(phrase, n)) continue;

          val de: PostingsEnum = MultiFields.getTermDocsEnum(ir, fieldName, rawPhrase)
          while (de.nextDoc != DocIdSetIterator.NO_MORE_DOCS) {
            val docId: Int = de.docID
            val freq: Int = de.freq
            // Let's consider only terms that occurs more than one time in the document
            // KEA papers said "To reduce the size of the training set, we discard any phrase that occurs only once in the document."

            if (freq >= minTF) {
              val pe: PostingsEnum = MultiFields.getTermPositionsEnum(ir, fieldName, rawPhrase)
              val ret: Int = pe.advance(docId)
              if (ret == DocIdSetIterator.NO_MORE_DOCS) {
                throw new RuntimeException("no more docs...")
              } else {
                // get first position of the term in the doc (first occurrence)
                val pos: Int = pe.nextPosition
                model.add(docId, fieldName, phrase, freq, pos)
              }
            }
          }
        })
      })
    } finally {
      if (ir != null)
        ir.close()
    }
    model
  }

  class KEAModel(val reader: IndexReader, val knownKeyphrases: Map[String, Set[String]]) {
    val searcher: IndexSearcher = new IndexSearcher(reader)
    val dmMap = scala.collection.mutable.Map.empty[String, KEADocModel]
    val docFreqMap = scala.collection.mutable.Map.empty[String, Integer]
    val docSizeMap = scala.collection.mutable.Map.empty[String, Integer]
    val docNum = reader.numDocs

    def add(docId: Int, fieldName: String, phrase: String, freq: Int, pos: Int) {
      val doc: Document = searcher.doc(docId)
      val contentId: String = doc.get(CONTENT_ID_FIELD_NAME)
      val docModel: KEADocModel = getDocModel(contentId)
      docModel.setPhraseFeatures(phrase, freq, pos, docSize(docId, contentId), docFreq(fieldName, phrase), docNum)
    }

    def getDocModel(contentId: String): KEADocModel = {
      dmMap.get(contentId) match {
        case Some(v) => v
        case None => {
          val dm = new KEADocModel
          dmMap.put(contentId, dm)
          dm
        }
      }
    }

    def docSize(docId: Int, contentId: String): Int = {
      docSizeMap.get(contentId) match {
        case Some(v) => v
        case None => {
          val size = searcher.doc(docId).get(DOC_SIZE_FIELD_NAME).toInt
          docSizeMap.put(contentId, size)
          size
        }
      }
    }

    def docFreq(fieldName: String, phrase: String): Int = {
      docFreqMap.get(phrase) match {
        case Some(v) => v
        case None => {
          val dfreq = reader.docFreq(new Term(fieldName, phrase))
          docFreqMap.put(phrase, dfreq)
          dfreq
        }
      }
    }

    def save(featuresFilename: String, keyphrasesFilename: String) {
      for (featuresOut <- managed(new PrintWriter(featuresFilename));
           keyphrasesOut <- managed(new PrintWriter(keyphrasesFilename))) {
        for (id <- dmMap.keySet.toSeq.sorted) {
          keyphrasesOut.println("")
          keyphrasesOut.println("* content id = %s".format(id))
          keyphrasesOut.println(" - doc size = %d".format(docSizeMap.get(id).get.toInt))
          keyphrasesOut.println(" - known keyphrases")
          val knowKeyphraseSet: Set[String] = knownKeyphrases(id)
          for (keyphrase <- knowKeyphraseSet) {
            keyphrasesOut.println(keyphrase)
          }
          keyphrasesOut.println(" - features")
          val docModel: KEADocModel = dmMap.get(id).get
          for (keyphrase <- docModel.ps.keySet) {
            val ps: PhraseStats = docModel.ps.get(keyphrase).get
            val class1: Boolean = knowKeyphraseSet.contains(keyphrase)
            keyphrasesOut.println("%s = %d %g %g".format(keyphrase, if (class1) 1 else 0, ps.tfidf, ps.firstOccurrence))
            featuresOut.println("%g %g %s".format(ps.tfidf, ps.firstOccurrence, class1))
          }
        }
      }
    }
  }

  class KEADocModel {
    val ps = scala.collection.mutable.Map.empty[String, PhraseStats]

    def setPhraseFeatures(phrase: String, freq: Int, pos: Int, docSize: Int, docFreq: Int, docNum: Int) {
      ps.put(phrase, new PhraseStats(freq, pos, docSize, docFreq, docNum))
    }
  }

  class PhraseStats(val freq: Int, val pos: Int, val docSize: Int, val docFreq: Int, val docNum: Int) {
    val tfidf = calcTfIdf(freq, docSize, docFreq, docNum)
    val firstOccurrence = calcFirstOccurrence(pos, docSize)
  }

  def transformToMDLPDiscretizationModel(featuresModelFile: String, cutpModelFile: String): Unit = {
    val lines: Seq[String] = FileUtils.readLines(new File(featuresModelFile))
    val featuresData: Seq[(Int, Map[Int, Double])] = lines.map {
      line => {
        val values: Array[String] = line.split("\\s")
        val bool = if (values(2).toBoolean) 1 else 0
        val v1 = values(0).toDouble
        val v2 = values(1).toDouble
        (bool, Map(0 -> v1, 1 -> v2))
      }
    }
    val sc = SparkContextLocal.newSparkContext("KEAModelBuildProcessor")
    val discretizer = try {
      val data: RDD[LabeledPoint] = sc
        .parallelize(featuresData)
        .map { case ((labelInt, values)) =>
          val features = Vectors.sparse(2, values.toSeq)
          val label = labelInt.toDouble
          LabeledPoint(label, features)
        }
      MDLPDiscretizer.train(data)
    }
    finally {
      if (sc != null)
        sc.stop()
    }
    for (cutpModelOut <- managed(new PrintWriter(cutpModelFile))) {
      cutpModelOut.println(discretizer.thresholds(0).map("%g".format(_)).mkString(" "))
      cutpModelOut.println(discretizer.thresholds(1).map("%g".format(_)).mkString(" "))
    }

  }

}

