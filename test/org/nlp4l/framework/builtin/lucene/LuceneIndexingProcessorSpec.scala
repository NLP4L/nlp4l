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

package org.nlp4l.framework.builtin.lucene

import java.io.{File, IOException, PrintWriter}

import com.typesafe.config.ConfigFactory
import org.apache.lucene.index.{IndexReader, MultiFields, Terms, TermsEnum}
import org.apache.lucene.util.BytesRef
import org.nlp4l.framework.models._
import org.nlp4l.lucene.RawReader
import org.specs2.mutable.{BeforeAfter, Specification}

class LuceneIndexingProcessorSpec extends Specification with BeforeAfter {

  val SCHEMA_TEXT = s"""
       |schema {
       |  defAnalyzer {
       |    class : org.apache.lucene.analysis.standard.StandardAnalyzer
       |  }
       |  fields = [
       |    {
       |      name : id
       |      indexed : true
       |      stored : true
       |    }
       |    {
       |      name : cat
       |      indexed : true
       |      stored : true
       |    }
       |    {
       |      name : body
       |      analyzer : {
       |        tokenizer {
       |          factory : standard
       |        }
       |        filters = [
       |          {
       |            factory : lowercase
       |          }
       |        ]
       |      }
       |      indexed : true
       |      stored : true
       |      termVector : true
       |      positions : true
       |      offsets : true
       |    }
       |  ]
       |}
        """.stripMargin

  val TMP_DIR = System.getProperty("java.io.tmpdir")

  val SCHEMA_PATH = new File(TMP_DIR, "nlp4l-iwriter-test-schema.conf").getAbsolutePath.replace('\\', '/')
  val INDEX_DIR1 = new File(TMP_DIR, "nlp4l-iwriter-test-index1").getAbsolutePath.replace('\\', '/')
  val INDEX_DIR2 = new File(TMP_DIR, "nlp4l-iwriter-test-index2").getAbsolutePath.replace('\\', '/')
  val INDEX_DIR3 = new File(TMP_DIR, "nlp4l-iwriter-test-index3").getAbsolutePath.replace('\\', '/')
  val INDEX_DIR4 = new File(TMP_DIR, "nlp4l-iwriter-test-index4").getAbsolutePath.replace('\\', '/')

  def before = {
  }

  def after = {
  }

  "LuceneIndexWriterProcessor" should {
    "execute with schemaFile " in {
      new PrintWriter(SCHEMA_PATH) { write(SCHEMA_TEXT); close }
      val config = ConfigFactory.parseString(
        s"""
          |{
          | index:      "${INDEX_DIR1}"
          | schemaFile: "${SCHEMA_PATH}"
          |}
        """.stripMargin)
      val proc = new LuceneIndexingProcessorFactory(config).getInstance()
      val dict = Dictionary(Seq(
        Record(Seq(
          Cell("id", "id-001"),
          Cell("cat", "cat-a"),
          Cell("body", "Hello. This is a body part of doc 001."))),
        Record(Seq(
          Cell("id", "id-002"),
          Cell("cat", "cat-b"),
          Cell("body", "Hello. This is a body part of doc 002.")))
      ))

      val result = proc.execute(Some(dict))
      val reader: RawReader = RawReader(config.getString("index"))
      try {
        reader.numFields mustEqual(3)
        reader.fieldMap.get("id")  must_!= None
        reader.fieldMap.get("cat")  must_!= None
        reader.fieldMap.get("body")  must_!= None
        reader.universalset().size mustEqual(2)
        reader.document(0).get.getValue("id") mustEqual (Some(List("id-001")))
        reader.document(0).get.getValue("cat") mustEqual (Some(List("cat-a")))
        reader.document(0).get.getValue("body") mustEqual (Some(List("Hello. This is a body part of doc 001.")))
        reader.document(1).get.getValue("id") mustEqual (Some(List("id-002")))
      } finally {
        if (reader != null)
          reader.close
      }
      result.get.recordList(0).cellValue("result").get mustEqual "success: 2 documents added."
    }

    "execute with schemaDef " in {
      val config = ConfigFactory.parseString(
        s"""
           |{
           | index:  "${INDEX_DIR2}"
           | schemaDef: {
           |   ${SCHEMA_TEXT}
           | }
           |}
        """.stripMargin)
      val proc = new LuceneIndexingProcessorFactory(config).getInstance()
      val dict = Dictionary(Seq(
        Record(Seq(
          Cell("id", "id-001"),
          Cell("cat", "cat-a"),
          Cell("body", "Hello. This is a body part of doc 001."))),
        Record(Seq(
          Cell("id", "id-002"),
          Cell("cat", "cat-b"),
          Cell("body", "Hello. This is a body part of doc 002.")))
      ))

      val result = proc.execute(Some(dict))
      val reader: RawReader = RawReader(config.getString("index"))
      try {
        reader.numFields mustEqual(3)
        reader.fieldMap.get("id")  must_!= None
        reader.fieldMap.get("cat")  must_!= None
        reader.fieldMap.get("body")  must_!= None
        reader.universalset().size mustEqual(2)
      } finally {
        if (reader != null)
          reader.close
      }
      true
    }

    "execute with setting options " in {
      val config1 = ConfigFactory.parseString(
        s"""
           |{
           | index:  "${INDEX_DIR3}"
           | schemaDef: {
           |   ${SCHEMA_TEXT}
           | }
           | deleteAll: true
           | optimize: false
           |}
        """.stripMargin)
      val proc1 = new LuceneIndexingProcessorFactory(config1).getInstance()
      val dict1 = Dictionary(Seq(
        Record(Seq(
          Cell("id", "id-001"),
          Cell("cat", "cat-a"),
          Cell("body", "Hello. This is a body part of doc 001."))),
        Record(Seq(
          Cell("id", "id-002"),
          Cell("cat", "cat-b"),
          Cell("body", "Hello. This is a body part of doc 002.")))
      ))
      val result1 = proc1.execute(Some(dict1))
      val reader1: RawReader = RawReader(config1.getString("index"))
      try {
        reader1.universalset().size mustEqual(2)
      } finally {
        if (reader1 != null)
          reader1.close
      }

      val config2 = ConfigFactory.parseString(
        s"""
           |{
           | index:  "${INDEX_DIR3}"
           | schemaDef: {
           |   ${SCHEMA_TEXT}
           | }
           | deleteAll: false
           | optimize: true
           |}
        """.stripMargin)
      val proc2 = new LuceneIndexingProcessorFactory(config2).getInstance()
      val dict2 = Dictionary(Seq(
        Record(Seq(
          Cell("id", "id-003"),
          Cell("cat", "cat-c"),
          Cell("body", "Hello. This is a body part of doc 003.")))
      ))
      val result2 = proc2.execute(Some(dict2))
      val reader2: RawReader = RawReader(config1.getString("index"))
      try {
        reader2.universalset().size mustEqual(3)
      } finally {
        if (reader1 != null)
          reader1.close
      }
      true
    }

    "execute with fieldsMap " in {
      val config = ConfigFactory.parseString(
        s"""
           |{
           | index:  "${INDEX_DIR4}"
           | schemaDef: {
           |   ${SCHEMA_TEXT}
           | }
           | fieldsMap: [
           |   { fieldName: "cat", cellName: "category" },
           |   { fieldName: "body", cellName: "contents" }
           | ]
           |}
        """.stripMargin)
      val proc = new LuceneIndexingProcessorFactory(config).getInstance()
      val dict = Dictionary(Seq(
        Record(Seq(
          Cell("id", "id-001"),
          Cell("category", "cat-a"),
          Cell("contents", "Hello. This is a body part of doc 001."))),
        Record(Seq(
          Cell("id", "id-002"),
          Cell("category", "cat-b"),
          Cell("contents", "Hello. This is a body part of doc 002.")))
      ))
      val result = proc.execute(Some(dict))
      val reader: RawReader = RawReader(config.getString("index"))
      try {
        reader.numFields mustEqual(3)
        reader.fieldMap.get("id")  must_!= None
        reader.fieldMap.get("cat")  must_!= None
        reader.fieldMap.get("body")  must_!= None
        reader.universalset().size mustEqual(2)
        reader.document(0).get.getValue("id") mustEqual (Some(List("id-001")))
        reader.document(0).get.getValue("cat") mustEqual (Some(List("cat-a")))
        reader.document(0).get.getValue("body") mustEqual (Some(List("Hello. This is a body part of doc 001.")))
        reader.document(1).get.getValue("id") mustEqual (Some(List("id-002")))
      } finally {
        if (reader != null)
          reader.close
      }
      true
    }

  }

}
