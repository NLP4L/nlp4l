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
import org.apache.lucene.index.{DirectoryReader, Terms, TermsEnum, _}
import org.apache.lucene.search.{DocIdSetIterator, IndexSearcher, TermQuery, TopDocs}
import org.apache.lucene.store.{Directory, FSDirectory}
import org.apache.lucene.util.BytesRef
import org.nlp4l.framework.models._
import org.nlp4l.framework.processors._
import play.api.Logger

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

class KeyphraseExtractionProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new KeyphraseExtractionProcessor(
      getStrParamRequired("idField"),
      getStrParamRequired("textField"),
      getStrParamRequired("modelDir"),
      getStrParam("keyphrasesSep", ","),
      getBoolParam("incrementDfDocNum", true),
      getStrParam("nGramPriorityOrder", "3,2,1"),
      getIntParam("maxKeyphrases", 20),
      getDoubleParam("scoreThreshold", 0.0),
      getStrListParam("passThruFields", null),
      getConfigParam("analyzer", null),
      getConfigParam("documentSizeAnalyzer", null),
      getIntParam("minTF", 2)
    )
  }
}

class KeyphraseExtractionProcessor(val idField: String,
                                   val textField: String,
                                   val modelDir: String,
                                   val keyphrasesSep: String,
                                   val incrementDfDocNum: Boolean,
                                   val nGramPriorityOrder: String,
                                   val maxKeyphrases: Int,
                                   val scoreThreshold: Double,
                                   val passThruFields: Seq[String],
                                   val analyzer: Config,
                                   val documentSizeAnalyzer: Config,
                                   val minTF: Int
                                  )
  extends Processor
    with CommonProcessor {

  val settings = new ModelDirSettings(modelDir)

  private val logger = Logger(this.getClass)

  override def execute(data: Option[Dictionary]): Option[Dictionary] = {
    val discr: Discretization = new Discretization(settings.CUTP_MODEL_FILE)
    val model: Model = new Model(settings.FEATURES_MODEL_FILE, discr)
    logger.debug(model.toString)

    indexing(settings.LUCENE_INDEX_EXTRACT_DIR, textField, data, analyzer, documentSizeAnalyzer, withTermVectors = true)

    val indexDir: Directory = FSDirectory.open(FileSystems.getDefault.getPath(settings.LUCENE_INDEX_EXTRACT_DIR))
    val ir: IndexReader = DirectoryReader.open(indexDir)
    val searcher: IndexSearcher = new IndexSearcher(ir)
    try {
      val resultRecords: ArrayBuffer[Record] = new ArrayBuffer[Record]()
      val indexModelDir: Directory = FSDirectory.open(FileSystems.getDefault.getPath(settings.LUCENE_INDEX_MODEL_DIR))
      val irModel: IndexReader = DirectoryReader.open(indexModelDir)
      try {
        data.get.recordList.zipWithIndex.foreach {
          case (record, i) =>
            val result: Seq[KeyphraseScore] = extractKeyphrases(i.toString, model, discr, incrementDfDocNum, nGramPriorityOrder, ir, searcher, irModel)
            for (kps <- result) {
              logger.debug(kps.toString)
            }
            val cells = new ArrayBuffer[Cell]
            // doc id
            cells += record.cellMap(idField)
            // classification
            cells += Cell("keyphrases", result.map(_.keyphrase).mkString(keyphrasesSep))
            // pass thru
            if (passThruFields != null) {
              passThruFields.foreach { passThruField =>
                cells += record.cellMap(passThruField)
              }
            }
            resultRecords += Record(cells.toSeq)
        }

      } finally {
        if (irModel != null)
          irModel.close()
      }
      Some(Dictionary(resultRecords.toSeq))
    } finally {
      if (ir != null)
        ir.close()
    }
  }

  def extractKeyphrases(contetId: String, model: Model, discr: Discretization, incrementDfDocNum: Boolean, nGramPriorityOrder: String, ir: IndexReader, searcher: IndexSearcher, irModel: IndexReader): Seq[KeyphraseScore] = {
    val kpsArray: ArrayBuffer[KeyphraseScore] = new ArrayBuffer[KeyphraseScore]()
    nGramPriorityOrder.split(",").map(_.toInt).foreach(n => {
      val fieldName: String = getFieldName(FIELD_NAME, n)
      val docId: Int = getDocId(searcher, contetId)
      val docSize: Int = getDocSize(searcher, docId)
      // get numDocs from the model index
      val numDocs: Int = irModel.numDocs
      val terms: Terms = ir.getTermVector(docId, fieldName)
      val te: TermsEnum = terms.iterator
      Iterator.continually(te.next()).takeWhile(_ != null).foreach(rawPhrase => {
        val phrase: String = rawPhrase.utf8ToString
        val features: Array[Double] = getFeatures(ir, fieldName, rawPhrase, docId, docSize, numDocs, irModel, incrementDfDocNum)
        if (features != null) {
          val kps = new KeyphraseScore(phrase, features, model, discr, n)
          if (kps.score >= scoreThreshold)
            kpsArray.add(kps)
        }
      })
    })

    val kpsResult: ArrayBuffer[KeyphraseScore] = new ArrayBuffer[KeyphraseScore]()
    val kpsScoredOrder = kpsArray.sortWith((o1, o2) =>
      if (o1.score == o2.score)
        if (o1.tfidf == o2.tfidf)
          nGramPriorityOrder.indexOf(o1.nGram) < nGramPriorityOrder.indexOf(o2.nGram)
        else
          o1.tfidf > o2.tfidf
      else
        o1.score > o2.score
    )
    kpsScoredOrder.foreach { kps =>
      if (!subphraseOf(kps, kpsResult))
        kpsResult.add(kps)
    }
    kpsResult.take(maxKeyphrases)
  }

  def subphraseOf(kps: KeyphraseScore, list: Seq[KeyphraseScore]): Boolean = {
    list.exists(_.keyphrase.indexOf(kps.keyphrase) >= 0)
  }

  def getDocId(searcher: IndexSearcher, contentId: String): Int = {
    val topDocs: TopDocs = searcher.search(new TermQuery(new Term(CONTENT_ID_FIELD_NAME, contentId)), 1)
    if (topDocs.totalHits == 0) throw new RuntimeException(contentId + " cannot be found in the index.")
    topDocs.scoreDocs(0).doc
  }

  def getDocSize(searcher: IndexSearcher, docId: Int): Int = {
    val doc: Document = searcher.doc(docId)
    doc.get(DOC_SIZE_FIELD_NAME).toInt
  }

  def getFeatures(ir: IndexReader, fieldName: String, rawPhrase: BytesRef, docId: Int, docSize: Int, numDocs: Int, irModel: IndexReader, inc: Boolean): Array[Double] = {
    val de: PostingsEnum = MultiFields.getTermDocsEnum(ir, fieldName, rawPhrase)
    val ret: Int = de.advance(docId)
    if (ret == DocIdSetIterator.NO_MORE_DOCS) {
      throw new RuntimeException("no more docs...")
    }
    else {
      val freq: Int = de.freq
      if (freq >= minTF) {
        val pe: PostingsEnum = MultiFields.getTermPositionsEnum(ir, fieldName, rawPhrase)
        val ret2: Int = pe.advance(docId)
        if (ret2 == DocIdSetIterator.NO_MORE_DOCS) {
          throw new RuntimeException("no more docs...")
        }
        else {
          val features: Array[Double] = new Array[Double](2)
          val pos: Int = pe.nextPosition
          // get docFreq from the model index
          val docFreq: Int = irModel.docFreq(new Term(fieldName, rawPhrase))
          features(0) = calcTfIdf(freq, docSize,
            if (inc) docFreq + 1 else docFreq,
            if (inc) numDocs + 1 else numDocs)
          features(1) = calcFirstOccurrence(pos, docSize)
          features
        }
      } else null
    }
  }

  class Discretization(val file: String) {

    val (featuresTfIdf: Seq[Double], featuresDistance: Seq[Double]) = readFile(file)

    def readFile(file: String): (Seq[Double], Seq[Double]) = {
      val br = new BufferedReader(new FileReader(new File(file)))
      try {
        val line1: String = br.readLine
        val line2: String = br.readLine
        val seq1 = line1.trim.split("\\s").map(_.toDouble).toSeq
        val seq2 = line2.trim.split("\\s").map(_.toDouble).toSeq
        (seq1, seq2)
      }
      finally {
        if (br != null)
          br.close()
      }
    }

    def discretizeTfIdf(value: Double): Int = {
      val result = featuresTfIdf.zipWithIndex.find {
        case (tfidf, i) => value < tfidf
      }
      if (result.isEmpty) featuresTfIdf.length else result.get._2
    }

    def discretizeDistance(value: Double): Int = {
      val result = featuresDistance.zipWithIndex.find {
        case (distance, i) => value < distance
      }
      if (result.isEmpty) featuresDistance.length else result.get._2
    }
  }

  class Model(val file: String, val discr: Discretization) {
    val numTfIdf: Int = discr.featuresTfIdf.length + 1
    val numDistance: Int = discr.featuresDistance.length + 1

    val countTfIdfYes: Array[Int] = new Array[Int](numTfIdf)
    val countTfIdfNo: Array[Int] = new Array[Int](numTfIdf)
    val countDistanceYes: Array[Int] = new Array[Int](numDistance)
    val countDistanceNo: Array[Int] = new Array[Int](numDistance)
    val countYesNo: Array[Int] = new Array[Int](2)

    val lines: Seq[String] = FileUtils.readLines(new File(file))
    lines.foreach { line => {
      val values: Array[String] = line.split("\\s")
      if (values(2).toBoolean) {
        countTfIdfYes(discr.discretizeTfIdf(values(0).toDouble)) += 1
        countDistanceYes(discr.discretizeDistance(values(1).toDouble)) += 1
        countYesNo(0) += 1
      }
      else {
        countTfIdfNo(discr.discretizeTfIdf(values(0).toDouble)) += 1
        countDistanceNo(discr.discretizeDistance(values(1).toDouble)) += 1
        countYesNo(1) += 1
      }
    }
    }
    val countYes: Int = countYesNo(0)
    val countNo: Int = countYesNo(1)

    val priorProbYes = countYes.toDouble / (countYes + countNo).toDouble
    val priorProbNo = countNo.toDouble / (countYes + countNo).toDouble

    val probTfIdfYes = new Array[Double](numTfIdf)
    val probTfIdfNo = new Array[Double](numTfIdf)
    val probDistanceYes = new Array[Double](numDistance)
    val probDistanceNo = new Array[Double](numDistance)

    (0 until numTfIdf).foreach(n => {
      probTfIdfYes(n) = countTfIdfYes(n).toDouble / countYes.toDouble
      probTfIdfNo(n) = countTfIdfNo(n).toDouble / countNo.toDouble
    })
    (0 until numDistance).foreach(n => {
      probDistanceYes(n) = countDistanceYes(n).toDouble / countYes.toDouble
      probDistanceNo(n) = countDistanceNo(n).toDouble / countNo.toDouble
    })

    override def toString: String = {
      val b = new StringBuilder
      b.append("model =")
      b.append("\ntfidf | yes    = ")
      for (p <- probTfIdfYes) {
        b.append(" %8.6f".format(p))
      }
      b.append("\ntfidf | no     = ")
      for (p <- probTfIdfNo) {
        b.append(" %8.6f".format(p))
      }
      b.append("\ndistance | yes = ")
      for (p <- probDistanceYes) {
        b.append(" %8.6f".format(p))
      }
      b.append("\ndistance | no  = ")
      for (p <- probDistanceNo) {
        b.append(" %8.6f".format(p))
      }
      b.append("\nP(yes) = %8.6f".format(priorProbYes))
      b.append("\nP(no)  = %8.6f".format(priorProbNo))
      b.append("\n")
      b.result()
    }
  }

  class KeyphraseScore(val keyphrase: String, val features: Array[Double], val model: Model, val discr: Discretization, val nGram: Int) {
    val tfidf: Double = features(0)
    val discrTfIdf: Int = discr.discretizeTfIdf(features(0))
    val discrDistance: Int = discr.discretizeDistance(features(1))
    val probYes: Double = model.priorProbYes * model.probTfIdfYes(discrTfIdf) * model.probDistanceYes(discrDistance)
    val probNo: Double = model.priorProbNo * model.probTfIdfNo(discrTfIdf) * model.probDistanceNo(discrDistance)
    val score: Double = probYes / (probYes + probNo)

    override def toString: String = {
      "%s (%8.6f,%8.6f)".format(keyphrase, score, tfidf)
    }
  }

}