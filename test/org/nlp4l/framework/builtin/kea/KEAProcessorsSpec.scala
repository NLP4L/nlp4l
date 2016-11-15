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

import java.io.File

import com.typesafe.config.ConfigFactory
import org.nlp4l.framework.models.{Record, _}
import org.specs2.mutable.{BeforeAfter, Specification}

class KEAProcessorsSpec extends Specification with BeforeAfter {

  object TestSettings {
    val TMP_DIR = System.getProperty("java.io.tmpdir")
    val TEST_OUT_DIR = new File(TMP_DIR, "nlp4l-kea-test").getAbsolutePath.replace('\\', '/')
  }

  def before = {
  }

  def after = {
  }

  "KEAModelBuildProcessor" should {
    "execute " in {

      // ------------------------------
      // KEAModelBuildProcessor
      // ------------------------------
      val config = ConfigFactory.parseString(
        s"""
           |{
           | textField:   "text"
           | keyphrasesField:  "keyphrases"
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | analyzer : {
           |   class: "org.nlp4l.framework.builtin.kea.KEAStandardAnalyzer"
           |   params {
           |     stopwords: "is, a, the, of"
           |   }
           | }
           | documentSizeAnalyzer : {
           |   tokenizer {
           |     factory : whitespace
           |   }
           | }
           |}
        """.stripMargin)
      val proc = new KEAModelBuildProcessorFactory(config).getInstance()
      val dict = Dictionary(Seq(
        Record(Seq(
          Cell("keyphrases", "cloud computing,internet"),
          Cell("text", "Cloud computing is internet based network. Internet is ... Cloud computing brings us ... internet world."))),
        Record(Seq(
          Cell("keyphrases", "out of memory"),
          Cell("text", "run out of memory. run out of memory. ")))
      ))

      val result = proc.execute(Some(dict))
      println(result)

      // ------------------------------
      // KeyphraseExtractionProcessor
      // ------------------------------
      val config2 = ConfigFactory.parseString(
        s"""
           |{
           | idField:   "id"
           | textField:  "text"
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | keyphrasesSep: "|"
//           | incrementDfDocNum: false
//           | nGramPriorityOrder: "1,2,3"
//           | scoreThreshold: 0.1
//           | maxKeyphrases: 10
           |}
        """.stripMargin)
      val proc2 = new KeyphraseExtractionProcessorFactory(config2).getInstance()
      val dict2 = Dictionary(Seq(
        Record(Seq(
          Cell("id", "DOC-001"),
          Cell("text", "Cloud computing is internet based network. Internet is ... Cloud computing brings us ... internet world."))),
        Record(Seq(
          Cell("id", "DOC-002"),
          Cell("text", "Cloud storage is internet based storage. Cloud storage stores data on the internet."))),
        Record(Seq(
          Cell("id", "DOC-003"),
          Cell("text", "run out of memory. run out of memory.")))
      ))

      val result2 = proc2.execute(Some(dict2))
      println(result2)

      result2.get.recordList.foreach(record =>
        println("keyphrases=" + record.cellValue("keyphrases").get)
      )

      true
    }

  }

}
