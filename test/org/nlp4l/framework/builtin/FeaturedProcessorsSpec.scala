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

package org.nlp4l.framework.builtin

import java.io.{StringReader, FileNotFoundException}

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.ja.dict.UserDictionary
import org.nlp4l.framework.models._
import org.specs2.mutable.Specification

class FeaturedProcessorsSpec extends Specification {

  "TextRecordsProcessor" should {
    "read solr.log file" in {
      val settings = ConfigFactory.parseString(
        """
          |{
          |  file : "test/resources/org/nlp4l/framework/builtin/solr.log"
          |  encoding : "UTF-8"
          |}
        """.stripMargin)
      val processor = new TextRecordsProcessorFactory(settings).getInstance
      val result: Dictionary = processor.execute(None).get
      result must_!= None
      result.recordList.size must_==(188)
    }

    "throw a FileNotFoundException" in {
      val settings = ConfigFactory.parseString(
        """
          |{
          |  file : "test/resources/org/nlp4l/framework/builtin/no_such_file"
          |  encoding : "UTF-8"
          |}
        """.stripMargin)
      val processor = new TextRecordsProcessorFactory(settings).getInstance
      processor.execute(None) must throwA[FileNotFoundException]
    }
  }

  "UniqueProcessor" should {
    "no records" in {
      val settings = ConfigFactory.parseString(
        """
          |{
          |  cellname : "uq"
          |}
        """.stripMargin)
      val processor = new UniqueProcessorFactory(settings).getInstance
      val result: Option[Dictionary] = processor.execute(None)
      result must_== None
    }

    "sorted records" in {
      val settings = ConfigFactory.parseString(
        """
          |{
          |  cellname : "uq"
          |}
        """.stripMargin)
      val processor = new UniqueProcessorFactory(settings).getInstance
      val data = Some(Dictionary(records("uq,text",
        """
          |aaa,check cell soon.
          |bbb,good and great.
          |bbb,not sure.
          |bbb,calendar for the next year.
          |ccc,he bought a new car.
          |ccc,natural language processing.
        """.stripMargin)))
      val result: Option[Dictionary] = processor.execute(data)
      result must_!= None
      result.get.recordList.size must_== 3
      result.get.recordList(0).cellValue("uq").get must_== "aaa"
      result.get.recordList(0).cellValue("text").get must_== "check cell soon."
      result.get.recordList(1).cellValue("uq").get must_== "bbb"
      result.get.recordList(1).cellValue("text").get must_== "good and great."
      result.get.recordList(2).cellValue("uq").get must_== "ccc"
      result.get.recordList(2).cellValue("text").get must_== "he bought a new car."
    }
  }

  def records(cellNames: String, data: String): Seq[Record] = {
    val cnames = cellNames.split(",")
    data.split("\n").filterNot(_.trim.length == 0).map{ r =>
      Record(r.split(",") zip cnames map { c =>
        Cell(c._2, c._1)
      })
    }
  }

  val solrLogProc = new StandardSolrQueryLogProcessorFactory(ConfigFactory.empty()).getInstance

  "StandardSolrQueryLogProcessor" should {
    "ignore records that don't match the pattern" in {
      solrLogProc.execute(Some(logRecord("aaa"))) must_== None
      val log =
        """2016-01-18 06:26:24.397
          | INFO  (searcherExecutor-7-thread-1-processing-x:collection1)
          |  [   x:collection1] o.a.s.c.SolrCore [collection1]
          |  Registered new searcher Searcher@41c3efab[collection1]
          |  main{ExitableDirectoryReader(UninvertingDirectoryReader(Uninverting(_0(5.3.1):C32)))}""".stripMargin
      solrLogProc.execute(Some(logRecord(log))) must_== None
    }

    "get single parameters" in {
      val log =
        """2016-01-18 06:27:31.343 INFO  (qtp1359044626-35) [   x:collection1] o.a.s.c.S.Request [collection1] webapp=/solr path=/select params={q=ipod+mini&fq=cat:cable} hits=0 status=0 QTime=6""".stripMargin
      val result = solrLogProc.execute(Some(logRecord(log)))
      result must_!= None
      checkCell(result, "date", "2016-01-18 06:27:31")
      checkCell(result, "q", "ipod+mini")
      checkCell(result, "fq", "cat:cable")
      checkCell(result, "hits", 0)
      checkCell(result, "QTime", 6)
    }

    "get multiple parameters" in {
      val log =
        """2016-01-18 06:27:31.343 INFO  (qtp1359044626-35) [   x:collection1] o.a.s.c.S.Request [collection1] webapp=/solr path=/select params={q=ipod+mini&fq=cat:cable&fq=price:[1 TO 100]&facet.field=cat&facet.field=author} hits=1000 status=0 QTime=123""".stripMargin
      val result = solrLogProc.execute(Some(logRecord(log)))
      result must_!= None
      checkCell(result, "date", "2016-01-18 06:27:31")
      checkCell(result, "q", "ipod+mini")
      checkCell(result, "fq", "cat:cable,price:[1 TO 100]")
      checkCell(result, "facet.field", "cat,author")
      checkCell(result, "hits", 1000)
      checkCell(result, "QTime", 123)
    }
  }

  def logRecord(s: String) = Record(Seq(Cell("text", s)))

  def checkCell(r: Option[Record], n: String, v: Any): Boolean = {
    if(r == None) false
    else checkCell(r.get, n, v)
  }

  def checkCell(r: Record, n: String, v: Any): Boolean = {
    val value = r.cellValue(n)
    if(value == None) false
    else value.get must_== v
  }

  val swCode =
    "a,an,and,are,as,at,be,but,by,for,if,in,into,is,it,no,not,of,on,or,such,that,the,their,then,there,these,they,this,to,was,will,with"
      .split(",").toSet

  "StopWordsUtil" should {

    "read stopwords.txt that contains comments and empty lines" in {
      val swFile = StopWordsUtil.stopwords("test/resources/org/nlp4l/framework/builtin/stopwords-simple.txt", "UTF-8", ",", 1)
      swFile must_== swCode
    }

    "read stopwords.txt that contains comments and empty lines and even column errors" in {
      val swFile = StopWordsUtil.stopwords("test/resources/org/nlp4l/framework/builtin/stopwords-multiplecolumns.txt", "UTF-8", ",", 3)
      swFile must_== swCode.filterNot(a => (a == "but") || (a == "for"))
    }
  }

  "StopWordsProcessor" should {
    "stopRecord must return false when it has no Cell that is named" in {
      val record = Record(Seq(Cell("name1", "val1"), Cell("name2", "val2")))
      val processor = new StopWordsProcessor(swCode, "mycell")
      processor.stopRecord(record) must_== false
    }

    "stopRecord must return false when it has Cell that is named but the value is not stop word" in {
      val record = Record(Seq(Cell("name1", "val1"), Cell("mycell", "val2")))
      val processor = new StopWordsProcessor(swCode, "mycell")
      processor.stopRecord(record) must_== false
    }

    "stopRecord must return true when it has a Cell that contain a stop word" in {
      val record = Record(Seq(Cell("name1", "val1"), Cell("mycell", "they")))
      val processor = new StopWordsProcessor(swCode, "mycell")
      processor.stopRecord(record) must_== true
    }

    "stopRecord must return true even if it has a numeric value" in {
      val record = Record(Seq(Cell("name1", "val1"), Cell("mycell", 10)))
      val processor = new StopWordsProcessor("2,4,6,8,10".split(",").toSet, "mycell")
      processor.stopRecord(record) must_== true
    }
  }

  "JaUserDictionaryProcessor" should {
    "generate terms and readings from a surface phrase" in {
      val proc = new JaUserDictionaryProcessor(null, "", "")
      val tr: (String, String) = proc.generateRecord("応用情報処理技術者")
      tr._1 must_== "応用 情報処理 技術 者"
      tr._2 must_== "オウヨウ ジョウホウショリ ギジュツ シャ"
    }

    "generate terms and readings from a sequence of surface phrases" in {
      val proc = new JaUserDictionaryProcessor(null, "", "")
      val tr1: (String, String) = proc.generateRecord("応用情報処理技術者")
      tr1._1 must_== "応用 情報処理 技術 者"
      tr1._2 must_== "オウヨウ ジョウホウショリ ギジュツ シャ"
      val tr2: (String, String) = proc.generateRecord("基本情報処理技術者")
      tr2._1 must_== "基本 情報処理 技術 者"
      tr2._2 must_== "キホン ジョウホウショリ ギジュツ シャ"
    }

    "work correctly even when the surface is unknown" in {
      val proc = new JaUserDictionaryProcessor(null, "", "")
      val tr: (String, String) = proc.generateRecord("フメイモジレツ")
      tr._1 must_== "フメイモジレツ"
      tr._2 must_== "NOREADING"
    }

    "work correctly even when the surface contains unknown terms" in {
      val proc = new JaUserDictionaryProcessor(null, "", "")
      val tr: (String, String) = proc.generateRecord("ブログ記事")
      tr._1 must_== "ブログ 記事"
      tr._2 must_== "NOREADING キジ"
    }

    "be applied user dictionary" in {
      val USER_DICT =
        """
          |応用情報処理技術者,応用情報処理技術者,オウヨウジョウホウショリギジュツシャ,資格名詞
          |基本情報処理技術者,基本情報処理技術者,キホンジョウホウショリギジュツシャ,資格名詞
        """.stripMargin
      val userdic = UserDictionary.open(new StringReader(USER_DICT))
      val proc = new JaUserDictionaryProcessor(userdic, "", "")
      val tr1: (String, String) = proc.generateRecord("応用情報処理技術者")
      tr1._1 must_== "応用情報処理技術者"
      tr1._2 must_== "オウヨウジョウホウショリギジュツシャ"
      val tr2: (String, String) = proc.generateRecord("基本情報処理技術者")
      tr2._1 must_== "基本情報処理技術者"
      tr2._2 must_== "キホンジョウホウショリギジュツシャ"
    }
  }

  "GenericDictionaryAttributeFactory" should {
    "construct with setting" in {
      val config = ConfigFactory.parseString(
        """
          |{
          | name: "MyDict"
          | attributes : [
          |   {
          |     name: "cell-a"
          |   },
          |   {
          |     name: "cell-b"
          |     cellType: "integer"
          |     isFilterable: false
          |   },
          |   {
          |     name: "cell-c"
          |     cellType: "double"
          |     isSortable: false
          |   },
          |   {
          |     name: "cell-d"
          |     cellType: "float"
          |   },
          |   {
          |     name: "cell-e"
          |     cellType: "date"
          |   }
          | ]
          |}
        """.stripMargin)

      val dict: DictionaryAttribute = new GenericDictionaryAttributeFactory(config).getInstance()

      dict.name mustEqual "MyDict"

      dict.cellAttributeList.length must_== (5)

      dict.cellAttributeList(0).name mustEqual "cell-a"
      dict.cellAttributeList(0).cellType must_== CellType.StringType
      dict.cellAttributeList(0).isFilterable must_== true
      dict.cellAttributeList(0).isSortable must_== true

      dict.cellAttributeList(1).name mustEqual "cell-b"
      dict.cellAttributeList(1).cellType must_== CellType.IntType
      dict.cellAttributeList(1).isFilterable must_== false
      dict.cellAttributeList(1).isSortable must_== true

      dict.cellAttributeList(2).name mustEqual "cell-c"
      dict.cellAttributeList(2).cellType must_== CellType.DoubleType
      dict.cellAttributeList(2).isFilterable must_== true
      dict.cellAttributeList(2).isSortable must_== false

      dict.cellAttributeList(3).name mustEqual "cell-d"
      dict.cellAttributeList(3).cellType must_== CellType.FloatType

      dict.cellAttributeList(4).name mustEqual "cell-e"
      dict.cellAttributeList(4).cellType must_== CellType.DateType
    }

    "throws an exception when a cellType is wrong" in {
      val config = ConfigFactory.parseString(
        """
          |{
          | attributes : [
          |   {
          |     name: "cell-a"
          |     cellType: "xxx"
          |   }
          | ]
          |}
        """.stripMargin)

      new GenericDictionaryAttributeFactory(config).getInstance() must throwA[IllegalArgumentException]
    }
  }
}
