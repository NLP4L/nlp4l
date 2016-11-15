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

package org.nlp4l.framework.builtin.acronym

import com.typesafe.config.ConfigFactory
import org.nlp4l.framework.models.{Cell, Record, Dictionary}
import org.specs2.mutable.Specification

import scala.collection.mutable.ArrayBuffer

class AcronymExtractionProcessorSpec extends Specification {

  "AcronymExtractionProcessor" should {

    val settings = ConfigFactory.parseString(
      """
        |{
        |  textField: "text"
        |  algorithm: simpleCanonical
        |}
      """.stripMargin)
    val processor = new AcronymExtractionProcessorFactory(settings).getInstance.asInstanceOf[AcronymExtractionProcessor]

    "stackWords short" in {
      processor.stackWords("1111  2222   333", false) must_== Array("1111", "2222", "333")
      processor.stackWords("1111  2222   333", true) must_== Array("333", "2222", "1111")
    }

    "stackWords long" in {
      processor.stackWords("1  2   3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23", false) must_==
        Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")
      processor.stackWords("1  2   3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23", true) must_==
        Array("23", "22", "21", "20", "19", "18", "17", "16", "15", "14", "13", "12", "11", "10", "9", "8", "7", "6", "5", "4")
    }

    "testUpperCaseStrict" in {
      val ACRONYM = "JAIST"
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("Japan", "Advanced", "Institute", "of", "Science", "and", "Technology"),
        false, false, ArrayBuffer.empty[String]) must_== Some("Japan Advanced Institute of Science and Technology")
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("Japan", "Advanced", "Institute", "Of", "Science", "And", "Technology"),
        false, false, ArrayBuffer.empty[String]) must_== Some("Japan Advanced Institute Of Science And Technology")
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("Japan", "advanced", "Institute", "of", "Science", "and", "Technology"),
        false, false, ArrayBuffer.empty[String]) must_== None
    }

    "testUpperCaseStrict reverse" in {
      val ACRONYM = "JAIST"
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("Japan", "Advanced", "Institute", "of", "Science", "and", "Technology").reverse,
        true, false, ArrayBuffer.empty[String]) must_== Some("Japan Advanced Institute of Science and Technology")
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("Japan", "Advanced", "Institute", "Of", "Science", "And", "Technology").reverse,
        true, false, ArrayBuffer.empty[String]) must_== Some("Japan Advanced Institute Of Science And Technology")
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("Japan", "advanced", "Institute", "of", "Science", "and", "Technology").reverse,
        true, false, ArrayBuffer.empty[String]) must_== None
    }

    "testUpperCaseStrict looseEnd" in {
      val ACRONYM = "JAIST"
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("Japan", "Advanced", "Institute", "of", "Science", "and", "Technology", "in", "Ishikawa"),
        false, true, ArrayBuffer.empty[String]) must_== Some("Japan Advanced Institute of Science and Technology")
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("Japan", "Advanced", "Institute", "Of", "Science", "And", "Technology", "In", "Ishikawa"),
        false, true, ArrayBuffer.empty[String]) must_== Some("Japan Advanced Institute Of Science And Technology")
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("Japan", "advanced", "Institute", "of", "Science", "and", "Technology", "in", "Ishikawa"),
        false, true, ArrayBuffer.empty[String]) must_== None
    }

    "testUpperCaseStrict reverse looseEnd" in {
      val ACRONYM = "JAIST"
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("Japan", "Advanced", "Institute", "of", "Science", "and", "Technology", "in", "Ishikawa").reverse,
        true, true, ArrayBuffer.empty[String]) must_== Some("Japan Advanced Institute of Science and Technology in Ishikawa")
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("Japan", "Advanced", "Institute", "Of", "Science", "And", "Technology", "In", "Ishikawa").reverse,
        true, true, ArrayBuffer.empty[String]) must_== Some("Japan Advanced Institute Of Science And Technology In Ishikawa")
      processor.testUpperCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("Japan", "advanced", "Institute", "of", "Science", "and", "Technology", "in", "Ishikawa").reverse,
        true, true, ArrayBuffer.empty[String]) must_== None
    }

    "testLowerCaseStrict" in {
      val ACRONYM = "JAIST"
      processor.testLowerCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("japan", "advanced", "institute", "of", "science", "and", "technology"),
        false, false, ArrayBuffer.empty[String]) must_== Some("japan advanced institute of science and technology")
      processor.testLowerCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("Japan", "advanced", "Institute", "of", "Science", "and", "Technology"),
        false, false, ArrayBuffer.empty[String]) must_== None
    }

    "testLowerCaseStrict reverse" in {
      val ACRONYM = "JAIST"
      processor.testLowerCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("japan", "advanced", "institute", "of", "science", "and", "technology").reverse,
        true, false, ArrayBuffer.empty[String]) must_== Some("japan advanced institute of science and technology")
      processor.testLowerCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("Japan", "advanced", "Institute", "of", "Science", "and", "Technology").reverse,
        true, false, ArrayBuffer.empty[String]) must_== None
    }

    "testLowerCaseStrict looseEnd" in {
      val ACRONYM = "JAIST"
      processor.testLowerCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("japan", "advanced", "institute", "of", "science", "and", "technology", "in", "Ishikawa"),
        false, true, ArrayBuffer.empty[String]) must_== Some("japan advanced institute of science and technology")
      processor.testLowerCaseStrict(ACRONYM, ACRONYM.toCharArray, Array("Japan", "advanced", "Institute", "of", "Science", "and", "Technology", "in", "Ishikawa"),
        false, true, ArrayBuffer.empty[String]) must_== None
    }

    "testLowerCaseStrict reverse looseEnd" in {
      val ACRONYM = "JAIST"
      processor.testLowerCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("japan", "advanced", "institute", "of", "science", "and", "technology", "in", "Ishikawa").reverse,
        true, true, ArrayBuffer.empty[String]) must_== Some("japan advanced institute of science and technology in Ishikawa")
      processor.testLowerCaseStrict(ACRONYM, ACRONYM.toCharArray.reverse, Array("Japan", "advanced", "Institute", "of", "Science", "and", "Technology", "in", "Ishikawa").reverse,
        true, true, ArrayBuffer.empty[String]) must_== None
    }

    "expansionCandidate in format of 'Expansion (ACRONYM)'" in {
      val ACRONYM = "ACRONYM"
      val TEXT = s"here is an expansion (${ACRONYM})"
      val start = TEXT.indexOf(ACRONYM)
      val length = ACRONYM.length
      val end = start + length
      val result = processor.expansionCandidate(TEXT, ACRONYM, start, end, length)
      result must_!= None
      result.get._1 must_== ACRONYM.toCharArray.reverse
      result.get._2 must_== Array("here","is","an","expansion").reverse
      result.get._3 must_== true
      result.get._4 must_== true
    }

    "expansionCandidate in format of 'Expansion (ACRONYM)' long version" in {
      val ACRONYM = "ACRONYM"
      val TEXT = s"aa bb cc dd ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt here is an expansion (${ACRONYM})"
      val start = TEXT.indexOf(ACRONYM)
      val length = ACRONYM.length
      val end = start + length
      val result = processor.expansionCandidate(TEXT, ACRONYM, start, end, length)
      result must_!= None
      result.get._1 must_== ACRONYM.toCharArray.reverse
      result.get._2 must_== "ee ff gg hh ii jj kk ll mm nn oo pp qq rr ss tt here is an expansion".split(" ").reverse
      result.get._3 must_== true
      result.get._4 must_== true
    }

    "expansionCandidate in format of 'Expansion or ACRONYM'" in {
      val ACRONYM = "ACRONYM"
      val TEXT = s"here is an expansion or ${ACRONYM}"
      val start = TEXT.indexOf(ACRONYM)
      val length = ACRONYM.length
      val end = start + length
      val result = processor.expansionCandidate(TEXT, ACRONYM, start, end, length)
      result must_!= None
      result.get._1 must_== ACRONYM.toCharArray.reverse
      result.get._2 must_== Array("here","is","an","expansion").reverse
      result.get._3 must_== true
      result.get._4 must_== true
    }

    "expansionCandidate in format of 'Expansion, or ACRONYM'" in {
      val ACRONYM = "ACRONYM"
      val TEXT = s"here is an expansion, or ${ACRONYM}"
      val start = TEXT.indexOf(ACRONYM)
      val length = ACRONYM.length
      val end = start + length
      val result = processor.expansionCandidate(TEXT, ACRONYM, start, end, length)
      result must_!= None
      result.get._1 must_== ACRONYM.toCharArray.reverse
      result.get._2 must_== Array("here","is","an","expansion").reverse
      result.get._3 must_== true
      result.get._4 must_== true
    }

    "expansionCandidate in format of 'Expansion, ACRONYM'" in {
      val ACRONYM = "ACRONYM"
      val TEXT = s"here is an expansion, ${ACRONYM}"
      val start = TEXT.indexOf(ACRONYM)
      val length = ACRONYM.length
      val end = start + length
      val result = processor.expansionCandidate(TEXT, ACRONYM, start, end, length)
      result must_!= None
      result.get._1 must_== ACRONYM.toCharArray.reverse
      result.get._2 must_== Array("here","is","an","expansion").reverse
      result.get._3 must_== true
      result.get._4 must_== true
    }

    "expansionCandidate in format of 'ACRONYM (Expansion)'" in {
      val ACRONYM = "ACRONYM"
      val TEXT = s"${ACRONYM} (here is an expansion)"
      val start = TEXT.indexOf(ACRONYM)
      val length = ACRONYM.length
      val end = start + length
      val result = processor.expansionCandidate(TEXT, ACRONYM, start, end, length)
      result must_!= None
      result.get._1 must_== ACRONYM.toCharArray
      result.get._2 must_== Array("here","is","an","expansion")
      result.get._3 must_== false
      result.get._4 must_== false
    }

    "expansionCandidate in format of 'ACRONYM, Expansion'" in {
      val ACRONYM = "ACRONYM"
      val TEXT = s"${ACRONYM}, here is an expansion and others"
      val start = TEXT.indexOf(ACRONYM)
      val length = ACRONYM.length
      val end = start + length
      val result = processor.expansionCandidate(TEXT, ACRONYM, start, end, length)
      result must_!= None
      result.get._1 must_== ACRONYM.toCharArray
      result.get._2 must_== Array("here","is","an","expansion","and","others")
      result.get._3 must_== false
      result.get._4 must_== true
    }

    "expansion" in {
      val TEXT =
        """
          |I sell my Compact Disc Read Only Memory (CD-ROM)
          |I graduated from JAIST, Japan Advanced Institute of Science and Technology.
          |You should go back home ASAP (as soon as possible).
        """.stripMargin
      checkDictionaryContent(
        """CD-ROM, Compact Disc Read Only Memory
          |JAIST, Japan Advanced Institute of Science and Technology
          |ASAP, as soon as possible
        """.stripMargin, processor.execute(textAsDictionary(TEXT)))
      ok
    }
  }

  def textAsDictionary(text: String): Option[Dictionary] = {
    Some(Dictionary(text.split("\n").map(r => Record(Seq(Cell("text", r))))))
  }

  def checkDictionaryContent(expected: String, actual: Option[Dictionary]): Unit = {
    val expectedRecords = expected.split("\n")
    actual must_!= None
    val dic = actual.get
    dic.recordList.zip(expectedRecords).foreach{ pair =>
      val cell = pair._1.cellValue("acronyms")
      cell must_!= None
      cell.get.toString must_== pair._2
    }
  }
}
