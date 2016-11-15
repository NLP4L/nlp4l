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

package org.nlp4l.framework.builtin.ner

import com.typesafe.config.Config
import opennlp.tools.namefind.{NameFinderME, TokenNameFinderModel}
import opennlp.tools.sentdetect.{SentenceDetectorME, SentenceModel}
import opennlp.tools.tokenize.{TokenizerME, TokenizerModel}
import opennlp.tools.util.Span
import org.nlp4l.framework.models.{DictionaryAttribute, Record, _}
import org.nlp4l.framework.processors._

import scala.collection.mutable.ArrayBuffer

class OpenNLPNerRecordProcessorFactory(settings: Config) extends RecordProcessorFactory(settings) {

  override def getInstance: RecordProcessor = {
    new OpenNLPNerRecordProcessor(
      getStrParamRequired("sentModel"),
      settings.getString("tokenModel"),
      getStrListParamRequired("nerModels"),
      getStrListParamRequired("nerTypes"),
      getStrListParamRequired("srcFields"),
      getStrParamRequired("idField"),
      getStrParam("separator", ","),
      getStrListParam("passThruFields", null))
  }
}

class OpenNLPNerRecordProcessor(val sentModel: String,
                                val tokenModel: String,
                                val nerModels: Seq[String],
                                val nerTypes: Seq[String],
                                val srcFields: Seq[String],
                                val idField: String,
                                val separator: String,
                                val passThruFields: Seq[String])
  extends RecordProcessor {

  val sentDetector = new SentenceDetectorME(new SentenceModel(new java.io.File(sentModel)))
  val tokenizer = new TokenizerME(new TokenizerModel(new java.io.File(tokenModel)))
  val extractors: Array[NameFinderME] = new Array[NameFinderME](nerModels.length)
  nerModels.zipWithIndex.foreach { case(nerModel, i) =>
     extractors(i) = new NameFinderME(new TokenNameFinderModel(new java.io.File(nerModel)))
  }

  override def execute(data: Option[Record]): Option[Record] = {
    data match {
      case Some(record) => {
        val cells = new ArrayBuffer[Cell]
        // doc id
        val docId = record.cellMap(idField).value
        cells += Cell(idField, docId)
        // each src
        srcFields.foreach { srcField =>
          val doc = record.cellMap(srcField).value.toString
          val sentences: Array[String] = sentDetector.sentDetect(doc)
          val sentenceTokens: Array[Array[String]] = new Array[Array[String]](sentences.length)
          sentences.zipWithIndex.foreach { case(sentence, sentIdx) =>
            sentenceTokens(sentIdx) = tokenizer.tokenize(sentence)
          }
          // each moodel(type)
          extractors.zipWithIndex.foreach { case (extractor, nerIndex) =>
            val result: ArrayBuffer[String] = new ArrayBuffer[String]()
            sentences.zipWithIndex.foreach {  case (sentence, sentIdx2) =>
              val tokens: Array[String] = sentenceTokens(sentIdx2)
              val names: Array[Span] = extractor.find(tokens)
              if (names != null && names.size > 0) {
                val entities: Array[String] = Span.spansToStrings(names, tokens)
                  entities.foreach { entity => {
                    result += entity
                  }
                }
              }
            }
            extractor.clearAdaptiveData()
            cells += Cell(srcField + "_" + nerTypes(nerIndex), result.mkString(separator))
          }
        }
        // pass thru
        if (passThruFields != null) {
          passThruFields.foreach { passThruField =>
            val cell = record.cellMap(passThruField)
            cells += cell
          }
        }
        Some(Record(cells.toSeq))
      }
      case _ => None
    }
  }

}
