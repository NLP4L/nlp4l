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

import java.io.File

import com.typesafe.config.ConfigFactory
import org.nlp4l.framework.models.{Cell, Dictionary, Record}
import org.specs2.mutable.{BeforeAfter, Specification}

import scala.collection.mutable.ArrayBuffer

class DocumentClassificationProcessorsSpec extends Specification with BeforeAfter {

  object TestSettings {
    val TMP_DIR = System.getProperty("java.io.tmpdir")
    val TEST_OUT_DIR = new File(TMP_DIR, "nlp4l-class-test").getAbsolutePath.replace('\\', '/')
  }
  def before = {
  }

  def after = {
  }
  "DocumentClassificationProcessors" should {
    "execute each processor as chain" in {

      val config = ConfigFactory.parseString(
        s"""
           |{
           | textField:   "text"
           | labelField:  "classification"
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | valuesFields:  ["classification", "text"]
           | analyzer : {
           |   tokenizer {
           |     factory : standard
           |   }
           |   filters = [
           |     {
           |       factory : lowercase
           |     }
           |   ]
           | }
//           | idfMode: "n"
           |}
        """.stripMargin)
      val records = new ArrayBuffer[Record]
      for(i <- 0 until 10) {
        records += Record(Seq(
          Cell("classification", "class-A"),
          Cell("text", "AAA AAA AAA AAA AAA AAA")))
      }
      for(i <- 0 until 10) {
        records += Record(Seq(
          Cell("classification", "class-B"),
          Cell("text", "BBB BBB BBB BBB CCC")))
      }
      for(i <- 0 until 10) {
        records += Record(Seq(
          Cell("classification", "class-C"),
          Cell("text", "CCC CCC CCC DDD")))
      }

      // Create LabeledPoint data
      val proc1 = new LabeledPointProcessorFactory(config).getInstance()
      val result1 = proc1.execute(Some(Dictionary(records)))
      println(result1)

      // Train and Model
      val config2 = ConfigFactory.parseString(
        s"""
           |{
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | algorithm:    "LogisticRegressionWithLBFGS"
           |}
        """.stripMargin)
      val proc2 = new TrainAndModelProcessorFactory(config2).getInstance()
      val result2 = proc2.execute(None)
      println(result2)

      // Predict and Classification
      val config3 = ConfigFactory.parseString(
        s"""
           |{
           | textField:   "text"
           | idField:     "docId"
           | passThruFields: [
           |   "title"
           |   ]
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | analyzer : {
           |   tokenizer {
           |     factory : standard
           |   }
           |   filters = [
           |     {
           |       factory : lowercase
           |     }
           |   ]
           | }
           | algorithm:    "LogisticRegressionWithLBFGS"
           |}
        """.stripMargin)
      val proc3 = new ClassificationProcessorFactory(config3).getInstance()
      val dict = Dictionary(Seq(
        Record(Seq(
          Cell("docId", "DOC-001"), Cell("title", "title 1"), Cell("text", "AAA AAA AAA"))),
        Record(Seq(
          Cell("docId", "DOC-002"), Cell("title", "title 2"), Cell("text", "AAA BBB BBB"))),
        Record(Seq(
          Cell("docId", "DOC-003"), Cell("title", "title 3"), Cell("text", "AAA CCC"))),
        Record(Seq(
          Cell("docId", "DOC-004"), Cell("title", "title 4"), Cell("text", "XXX XXX"))),
        //
        Record(Seq(
          Cell("docId", "DOC-003"), Cell("title", "title 3"), Cell("text", "AAA"))),
        Record(Seq(
          Cell("docId", "DOC-003"), Cell("title", "title 3"), Cell("text", "BBB"))),
        Record(Seq(
          Cell("docId", "DOC-003"), Cell("title", "title 3"), Cell("text", "CCC"))),
        Record(Seq(
          Cell("docId", "DOC-003"), Cell("title", "title 3"), Cell("text", "DDD"))),
        Record(Seq(
          Cell("docId", "DOC-003"), Cell("title", "title 3"), Cell("text", "EEE")))
      ))
      val result3 = proc3.execute(Some(dict))
      println(result3)

      result3.get.recordList(0).cellList(1).value mustEqual "class-A"
      result3.get.recordList(1).cellList(1).value mustEqual "class-B"
      result3.get.recordList(2).cellList(1).value mustEqual "class-C"
      result3.get.recordList(3).cellList(1).value mustEqual "class-A"

      result3.get.recordList(4).cellList(1).value mustEqual "class-A"
      result3.get.recordList(5).cellList(1).value mustEqual "class-B"
      result3.get.recordList(6).cellList(1).value mustEqual "class-C"
      result3.get.recordList(7).cellList(1).value mustEqual "class-C"
      result3.get.recordList(8).cellList(1).value mustEqual "class-A"

      result3.get.recordList(0).cellList(0).name mustEqual "docId"
      result3.get.recordList(0).cellList(0).value mustEqual "DOC-001"
      result3.get.recordList(0).cellList(1).name mustEqual "classification"
      result3.get.recordList(0).cellList(1).value mustEqual "class-A"
      result3.get.recordList(0).cellList(2).name mustEqual "title"
      result3.get.recordList(0).cellList(2).value mustEqual "title 1"

    }
  }

}
