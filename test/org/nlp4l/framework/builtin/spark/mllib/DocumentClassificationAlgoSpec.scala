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

class DocumentClassificationAlgoSpec extends Specification with BeforeAfter {

  object TestSettings {
    val TMP_DIR = System.getProperty("java.io.tmpdir")
    val TEST_OUT_DIR = new File(TMP_DIR, "nlp4l-class-algo-test").getAbsolutePath.replace('\\', '/')
  }
  def before = {
  }

  def after = {
  }
  "DocumentClassificationAlgo" should {
    "execute each algorithm" in {

      val records = new ArrayBuffer[Record]
      for(i <- 0 until 10) {
        records += Record(Seq(
          Cell("classification", "class-A"),
          Cell("text", "AAA AAA AAA")))
      }
      for(i <- 0 until 10) {
        records += Record(Seq(
          Cell("classification", "class-B"),
          Cell("text", "BBB BBB BBB")))
      }
      for(i <- 0 until 10) {
        records += Record(Seq(
          Cell("classification", "class-C"),
          Cell("text", "CCC CCC CCC")))
      }

      // Create LabeledPoint data
      val proc1 = new LabeledPointProcessorFactory(ConfigFactory.parseString(
        s"""
           |{
           | textField:   "text"
           | labelField:  "classification"
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | analyzer {
           |   class : org.apache.lucene.analysis.standard.StandardAnalyzer
           | }
           | idfMode: "n"
           |}
        """.stripMargin)).getInstance()
      val result = proc1.execute(Some(Dictionary(records)))
      println(result)

      val dict = Dictionary(Seq(
        Record(Seq(
          Cell("docId", "DOC-001"), Cell("title", "title 1"), Cell("text", "AAA AAA"))),
        Record(Seq(
          Cell("docId", "DOC-002"), Cell("title", "title 2"), Cell("text", "BBB BBB"))),
        Record(Seq(
          Cell("docId", "DOC-003"), Cell("title", "title 3"), Cell("text", "CCC CCC")))
      ))

      //
      // NaiveBayes
      //
      println(
        new TrainAndModelProcessorFactory(ConfigFactory.parseString(
          s"""
             |{
             | modelDir:   "${TestSettings.TEST_OUT_DIR}"
             | algorithm:    "NaiveBayes"
             | algorithmParams {
             |   lambda: 0.9
             |   modelType: "multinomial"
             | }
             |}
          """.stripMargin)).getInstance().execute(None)
      )
      val result1 = new ClassificationProcessorFactory(ConfigFactory.parseString(
        s"""
           |{
           | textField:   "text"
           | idField:     "docId"
           | passThruFields: [
           |   "title"
           |   ]
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           |  analyzer {
           |   class : org.apache.lucene.analysis.standard.StandardAnalyzer
           | }
           | algorithm:    "NaiveBayes"
           |}
        """.stripMargin)).getInstance().execute(Some(dict))
      println(result1)
      result1.get.recordList(0).cellList(1).value mustEqual "class-A"
      result1.get.recordList(1).cellList(1).value mustEqual "class-B"
      result1.get.recordList(2).cellList(1).value mustEqual "class-C"

      //
      // LogisticRegressionWithLBFGS
      //
      println(
        new TrainAndModelProcessorFactory(ConfigFactory.parseString(
          s"""
             |{
             | modelDir:   "${TestSettings.TEST_OUT_DIR}"
             | algorithm:    "LogisticRegressionWithLBFGS"
             |}
          """.stripMargin)).getInstance().execute(None)
      )
      val result2 = new ClassificationProcessorFactory(ConfigFactory.parseString(
        s"""
           |{
           | textField:   "text"
           | idField:     "docId"
           | passThruFields: [
           |   "title"
           |   ]
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | analyzer {
           |   class : org.apache.lucene.analysis.standard.StandardAnalyzer
           | }
           | algorithm:    "LogisticRegressionWithLBFGS"
           |}
        """.stripMargin)).getInstance().execute(Some(dict))
      println(result2)
      result2.get.recordList(0).cellList(1).value mustEqual "class-A"
      result2.get.recordList(1).cellList(1).value mustEqual "class-B"
      result2.get.recordList(2).cellList(1).value mustEqual "class-C"

      //
      // DecisionTree
      //
      println(
        new TrainAndModelProcessorFactory(ConfigFactory.parseString(
          s"""
             |{
             | modelDir:   "${TestSettings.TEST_OUT_DIR}"
             | algorithm:    "DecisionTree"
             | algorithmParams {
             |   impurity: "gini"
             |   maxDepth: 6
             |   maxBins: 33
             | }
             |}
          """.stripMargin)).getInstance().execute(None)
      )
      val result3 = new ClassificationProcessorFactory(ConfigFactory.parseString(
        s"""
           |{
           | textField:   "text"
           | idField:     "docId"
           | passThruFields: [
           |   "title"
           |   ]
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | analyzer {
           |   class : org.apache.lucene.analysis.standard.StandardAnalyzer
           | }
           | algorithm:    "DecisionTree"
           |}
        """.stripMargin)).getInstance().execute(Some(dict))
      println(result3)
      result3.get.recordList(0).cellList(1).value mustEqual "class-A"
      result3.get.recordList(1).cellList(1).value mustEqual "class-B"
      result3.get.recordList(2).cellList(1).value mustEqual "class-C"

      //
      // RandomForest
      //
      println(
        new TrainAndModelProcessorFactory(ConfigFactory.parseString(
          s"""
             |{
             | modelDir:   "${TestSettings.TEST_OUT_DIR}"
             | algorithm:    "RandomForest"
             | algorithmParams {
             |   numTrees: 9
             |   featureSubsetStrategy: "sqrt"
             |   impurity: "gini"
             |   maxDepth: 5
             |   maxBins: 101
             | }
             |}
          """.stripMargin)).getInstance().execute(None)
      )
      val result4 = new ClassificationProcessorFactory(ConfigFactory.parseString(
        s"""
           |{
           | textField:   "text"
           | idField:     "docId"
           | passThruFields: [
           |   "title"
           |   ]
           | modelDir:   "${TestSettings.TEST_OUT_DIR}"
           | analyzer {
           |   class : org.apache.lucene.analysis.standard.StandardAnalyzer
           | }
           | algorithm:    "RandomForest"
           |}
        """.stripMargin)).getInstance().execute(Some(dict))
      println(result4)
      result4.get.recordList(0).cellList(1).value mustEqual "class-A"
      result4.get.recordList(1).cellList(1).value mustEqual "class-B"
      result4.get.recordList(2).cellList(1).value mustEqual "class-C"
    }
  }

}
