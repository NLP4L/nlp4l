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

import com.typesafe.config.Config
import org.nlp4l.framework.models.{Record, Cell, Dictionary}
import org.nlp4l.framework.processors._
import play.api.Logger

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class AcronymExtractionProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new AcronymExtractionProcessor(getStrParamRequired("textField"))
  }
}

class AcronymExtractionProcessor(val textField: String) extends Processor {

  private val logger = Logger(this.getClass)
  private val regexAcronym = """[A-Z][A-Za-z/\-]*[A-Z]""".r
  private val MAX_WORDS = 20

  override def execute(data: Option[Dictionary]): Option[Dictionary] = {
    data match {
      case Some(dic) => {
        val result = ListBuffer.empty[Record]
        dic.recordList.foreach( record => {
          val value = record.cellValue(textField)
          if(value != None){
            val text = value.get.toString
            regexAcronym.findAllIn(text).matchData.foreach( m => {
              val length = m.end - m.start
              if(length <= 10){
                val rec = expansion(text, m.toString, m.start, m.end, length)
                if(rec != None)
                  result += Record(Seq(Cell("acronyms", s"${m.toString}, ${rec.get}")))
              }
            })
          }
        })
        Some(Dictionary(result))
      }
      case _ => None
    }
  }

  def expansion(text: String, acronym: String, start: Int, end: Int, length: Int): Option[String] = {
    expansionCandidate(text, acronym, start, end, length) match {
      case Some(ec) => {
        val ac = ec._1
        val as = ec._2
        val reverse = ec._3
        val looseEnd = ec._4
        if(as.length >= 2){
          testUpperCaseStrict(acronym, ac, as, reverse, looseEnd, ArrayBuffer.empty[String]) match {
            case Some(expansion) => Some(expansion)
            case _ => {
              testLowerCaseStrict(acronym, ac, as, reverse, looseEnd, ArrayBuffer.empty[String]) match {
                case Some(expansion) => Some(expansion)
                case _ => {
                  testUpperCaseLoose(acronym, ac, as, reverse, looseEnd) match {
                    case Some(expansion) => Some(expansion)
                    case _ => None
                  }
                }
              }
            }
          }
        }
        else None
      }
      case _ => None
    }
  }

  def stackWords(str: String, reverse: Boolean): Array[String] = {
    val words = str.trim.split("""\s+""")
    if(reverse) words.reverse.take(MAX_WORDS) else words.take(MAX_WORDS)
  }

  def expansionCandidate(text: String, acronym: String, start: Int, end: Int, length: Int): Option[(Array[Char], Array[String], Boolean, Boolean)] = {
    // check for "Expansion (ACRONYM)"
    // ex) He is the Chief Executive Officer (CEO)
    //     0123456789012345678901234567890123456789
    //               1         2         3
    val test1 = if(text.length > end){
      val regex = """([A-Za-z/\-\s]+) \(%s\)$""".format(acronym).r
      val src = text.substring(0, end + 1)
      regex.findFirstMatchIn(src) match {
        case Some(m) => {
          val reverse = true
          val ac = acronym.toCharArray.reverse
          val as = stackWords(m.group(1), reverse)
          Some((ac, as, reverse, true))
        }
        case _ => {
          None
        }
      }
    }
    else None

    // check for "Expansion or ACRONYM" and "Expansion, or ACRONYM"
    // ex) He is the Chief Executive Officer or CEO
    val test2 = if(test1 != None) test1
    else{
      val regex = """([A-Za-z/\-\s]+),? or %s$""".format(acronym).r
      val src = text.substring(0, end)
      regex.findFirstMatchIn(src) match {
        case Some(m) => {
          val reverse = true
          val ac = acronym.toCharArray.reverse
          val as = stackWords(m.group(1), reverse)
          Some((ac, as, reverse, true))
        }
        case _ => {
          None
        }
      }
    }

    // check for "Expansion, ACRONYM"
    // ex) He is the Chief Executive Officer, CEO
    val test3 = if(test2 != None) test2
    else{
      val regex = """([A-Za-z/\-\s]+), %s$""".format(acronym).r
      val src = text.substring(0, end)
      regex.findFirstMatchIn(src) match {
        case Some(m) => {
          val reverse = true
          val ac = acronym.toCharArray.reverse
          val as = stackWords(m.group(1), reverse)
          Some((ac, as, reverse, true))
        }
        case _ => {
          None
        }
      }
    }

    // check for "ACRONYM (Expansion)"
    // ex) He is the CEO (Chief Executive Officer).
    val test4 = if(test3 != None) test3
    else{
      val regex = """^%s \(([A-Za-z/\-\s]+)\)""".format(acronym).r
      val src = text.substring(start)
      regex.findFirstMatchIn(src) match {
        case Some(m) => {
          val reverse = false
          val ac = acronym.toCharArray
          val as = stackWords(m.group(1), reverse)
          Some((ac, as, reverse, false))
        }
        case _ => {
          None
        }
      }
    }


    // check for "ACRONYM, Expansion"
    // ex) He is the CEO, Chief Executive Officer.
    val test5 = if(test4 != None) test4
    else{
      val regex = """^%s, ([A-Za-z/\-\s]+)""".format(acronym).r
      val src = text.substring(start)
      regex.findFirstMatchIn(src) match {
        case Some(m) => {
          val reverse = false
          val ac = acronym.toCharArray
          val as = stackWords(m.group(1), reverse)
          Some((ac, as, reverse, true))
        }
        case _ => {
          None
        }
      }
    }

    test5
  }

  def testUpperCaseStrict(acronym: String, ac: Array[Char], as: Array[String], reverse: Boolean, looseEnd: Boolean, temp: ArrayBuffer[String]): Option[String] = {
    if(ac.size == 0){
      if(looseEnd || (!looseEnd && as.size == 0)){
        val str = if(reverse) temp.reverse.mkString(" ")
        else temp.mkString(" ")
        if(str.charAt(0) == acronym.charAt(0)) Some(str)
        else None
      }
      else{
        None
      }
    } else if(as.size == 0){
      None
    } else{
      val c = ac(0).toUpper
      val word = as(0)
      if(c == word(0)){
        temp += word
        testUpperCaseStrict(acronym, ac.tail.dropWhile{ a => a == '-' || a == '/'}, as.tail, reverse, looseEnd, temp)
      }
      else{
        temp += word
        testUpperCaseStrict(acronym, ac, as.tail, reverse, looseEnd, temp)
      }
    }
  }

  def testLowerCaseStrict(acronym: String, ac: Array[Char], as: Array[String], reverse: Boolean, looseEnd: Boolean, temp: ArrayBuffer[String]): Option[String] = {
    if(ac.size == 0){
      if(looseEnd || (!looseEnd && as.size == 0)){
        val str = if(reverse) temp.reverse.mkString(" ")
        else temp.mkString(" ")
        if(str.charAt(0) == acronym.charAt(0).toLower) Some(str)
        else None
      }
      else{
        None
      }
    } else if(as.size == 0){
      None
    } else{
      val c = ac(0).toLower
      val word = as(0)
      if(c == word(0)){
        temp += word
        testLowerCaseStrict(acronym, ac.tail.dropWhile{ a => a == '-' || a == '/'}, as.tail, reverse, looseEnd, temp)
      }
      else{
        temp += word
        testLowerCaseStrict(acronym, ac, as.tail, reverse, looseEnd, temp)
      }
    }
  }

  def testUpperCaseLoose(acronym: String, ac: Array[Char], as: Array[String], reverse: Boolean, looseEnd: Boolean): Option[String] = {
    if(!looseEnd){
      val expWords = if(reverse) as.reverse else as
      if(acronym.charAt(0) == expWords(0).charAt(0) && ac.contains(expWords(expWords.length - 1).charAt(0))) Some(expWords.mkString(" "))
      else None
    }
    else{
      val firstLetter = acronym.charAt(0)
      if(reverse){
        if(ac.contains(as(0).charAt(0))){
          // it is safe to use takeWhile() after using tail() because as.length is always larger than 1
          val intermediateWords = as.tail.takeWhile(_.charAt(0) != firstLetter)
          if(as.length >= intermediateWords.length + 2) Some(Array(Array(as(0)),intermediateWords,Array(as(intermediateWords.length + 1))).flatten.reverse.mkString(" "))
          else None
        }
        else None
      }
      else {
        if(as(0).charAt(0) == firstLetter){
          // it is safe to use takeWhile() after using tail() because as.length is always larger than 1
          val intermediateWords = as.tail.takeWhile(a => !ac.contains(a.charAt(0)))
          if(as.length >= intermediateWords.length + 2) Some(Array(Array(as(0)),intermediateWords,Array(as(intermediateWords.length + 1))).flatten.mkString(" "))
          else None
        }
        else None
      }
    }
  }
}
