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

import com.typesafe.config.Config
import org.apache.solr.client.solrj.impl.HttpSolrClient
import org.apache.solr.client.solrj.request.QueryRequest
import org.apache.solr.common.params.ModifiableSolrParams
import org.nlp4l.framework.models.Dictionary
import org.nlp4l.framework.processors.{Validator, ValidatorFactory}
import play.api.Logger

import scala.collection.mutable.ArrayBuffer

class UniqueRecordValidatorFactory(settings: Config) extends ValidatorFactory(settings) {
  override def getInstance: Validator = {
    new UniqueRecordValidator(getStrParamRequired("cellName"))
  }
}

class UniqueRecordValidator(val cellname: String) extends Validator {

  val logger = Logger(this.getClass)

  override def validate (data: Option[Dictionary]): Tuple2[Boolean, Seq[String]] = {
    data match {
      case Some(dic) => {
        try{
          val list = dic.cellList[String](cellname, a => a.toString)
          unique(list.toList) match {
            case Some(value) => {
              val msg = s"""cell value '$value' is found among records multiple times"""
              (false, Seq(msg))
            }
            case _ => (true, Seq())
          }
        }
        catch{
          case e => {
            val msg = s"cell name '$cellname' not found in some Records"
            logger.warn(msg)
            (false, Seq(msg, e.getMessage))
          }
        }
      }
      case None => { (true, Seq()) }
    }
  }

  def unique(list: List[String]): Option[String] = {
    list match {
      case a :: b => {
        if(b.contains(a)) Some(a)
        else unique(b)
      }
      case Nil => None
    }
  }
}

class RegexValidatorFactory(settings: Config) extends ValidatorFactory(settings) {
  override def getInstance: Validator = {
    val accept = getStrParam("regexAccept", null)
    val deny = getStrParam("regexDeny", null)
    (accept, deny) match {
      case (null, null) => throw new IllegalArgumentException(s"either regexAccept or regexDeny must be set")
      case _ => {
        new RegexValidator(getStrParamRequired("cellName"), accept, deny)
      }
    }
  }
}

class RegexValidator(val cellname: String, val accept: String, val deny: String) extends Validator {

  val logger = Logger(this.getClass)

  override def validate (data: Option[Dictionary]): Tuple2[Boolean, Seq[String]] = {
    data match {
      case Some(dic) => {
        val list = dic.cellList[String](cellname, a => a.toString)
        val result = checkPatterns(list)
        if(result._1) (true, Seq())
        else (false, Seq(result._2))
      }
      case None => (true, Seq())
    }
  }

  def checkPatterns(cellList: Seq[String]): (Boolean, String) = {
    (accept, deny) match {
      case (null, null) => (true, "")
      case (a, null) => checkAccept(cellList)
      case (null, b) => checkDeny(cellList)
      case (a, b) => {
        val resultAccept = checkAccept(cellList)
        if(resultAccept._1 == false) resultAccept
        else checkDeny(cellList)
      }
    }
  }

  private def checkAccept(cellList: Seq[String]): (Boolean, String) = {
    val regex = accept.r
    val result = cellList.filter(c => regex.findFirstIn(c) == None)
    if(result.length > 0) (false, s"""cell '${result.head}' of $cellname is not acceptable""")
    else (true, "")
  }

  private def checkDeny(cellList: Seq[String]): (Boolean, String) = {
    val regex = deny.r
    val result = cellList.filter(c => regex.findFirstIn(c) != None)
    if(result.length > 0) (false, s"""cell '${result.head}' of $cellname is denied""")
    else (true, "")
  }
}

class SolrSearchValidatorFactory(settings: Config) extends ValidatorFactory(settings) {
  override def getInstance: Validator = {
    val validateIn = getStrParamRequired("validateIn")
    val collection = getStrParamRequired("collection")
    val field = getStrParamRequired("field")
    val cellName = getStrParamRequired("cellName")
    val maxInvalids = getIntParam("maxInvalids", 10)
    val separatedBy = getStrParam("separatedBy", null)
    new SolrSearchValidator(validateIn, collection, field, cellName, maxInvalids, separatedBy)
  }
}

class SolrSearchValidator(val url: String, val collection: String, val field: String, val cellName: String,
                          val maxInvalids: Int, val separatedBy: String) extends Validator {
  val logger = Logger(this.getClass)
  override def validate (data: Option[Dictionary]): Tuple2[Boolean, Seq[String]] = {
    logger.info(s"url = $url collection = $collection, field = $field, cellName = $cellName, maxInvalids = $maxInvalids, separatedBy = $separatedBy")
    data match {
      case Some(dic) => {
        var count = 0
        val notFounds = ArrayBuffer.empty[String]
        dic.recordList.foreach{ r =>
          try{
            val solr = new HttpSolrClient(url)
            r.cellValue(cellName) match {
              case Some(q) => {
                val queries = if(separatedBy != null){
                  q.toString.split(separatedBy).map(a => a.trim)
                }
                else{
                  Array(q.toString)
                }
                queries.foreach{ query =>
                  val params = new ModifiableSolrParams().add("q", query.toString).add("rows", "0")
                  val req = new QueryRequest(params)
                  val res = solr.query(collection, params)
                  if(res.getResults.getNumFound == 0){
                    notFounds += query.toString
                    count = count + 1
                    logger.warn(s"($count/$maxInvalids) $query cannot be found in $url")
                  }
                }
              }
              case None => {}
            }
            if(count >= maxInvalids) return (false, Seq(s"The following terms are not found in the field '$field'", notFounds.mkString(",")))
          }
          catch {
            case e: Exception => {
              logger.error(e.getMessage)
              (false, Seq(e.getMessage))
            }
          }
        }
        if(count == 0) (true, Seq())
        else (false, Seq(s"The following terms are not found in the field '$field'", notFounds.mkString(",")))
      }
      case None => (true, Seq())
    }
  }
}
