/*
 * Copyright 2015 org.NLP4L
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

package org.nlp4l.ltr.support.procs

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.TimeoutException
import scala.util.Failure
import scala.util.Success

import org.nlp4l.ltr.support.actors.FeatureExtractSetResultMsg
import org.nlp4l.ltr.support.actors.FeatureProgressReport
import org.nlp4l.ltr.support.models.DocFeature
import org.nlp4l.ltr.support.models.FeatureExtractDTOs
import org.nlp4l.ltr.support.models.FeatureExtractParameter
import org.nlp4l.ltr.support.models.FeatureExtractQueries
import org.nlp4l.ltr.support.models.FeatureExtractResult
import org.nlp4l.ltr.support.models.FeatureExtractResults
import org.nlp4l.ltr.support.models.LtrModels._

import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import dispatch.Http
import dispatch.as
import dispatch.implyRequestHandlerTuple
import dispatch.url
import play.api.Logger
import play.api.libs.json.JsLookupResult.jsLookupResultToJsLookup
import play.api.libs.json.JsValue
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json




class FeatureExtractor(sender: ActorRef) extends FeatureProgressReport {
  
  private val logger = Logger(this.getClass)
  
  def execute(dtos: FeatureExtractDTOs) = {  
    // Post the annotated docs and queries
    val param = FeatureExtractQueries(dtos.idField, dtos.dtos )
    val extractUrl = dtos.featureExtractUrl + s"?command=extract&conf=${dtos.featureExtractConfig}&wt=json"
    val s_req = url(extractUrl).POST
          .setBody(Json.toJson(param).toString())
          .setHeader("Accept", "application/json")
          .setHeader("Content-Type", "application/json; charset=utf-8")
    s_req.subject.underlying { _.setBodyEncoding("UTF-8") }
    val s_f = Http(s_req OK as.String)
    Await.ready(s_f, scala.concurrent.duration.Duration.Inf)
    s_f.value.get match {
      case Success(s_res) => {
        // Progress
        val s_res_json: JsValue = Json.parse(s_res)
        val procid = (s_res_json \ "results" \ "procId").as[Long]
        val progressUrl = dtos.featureExtractUrl + s"?command=progress&procId=${procid.toString}&wt=json"

        val p_req = url(progressUrl)
        
        var progressV: Int = 0
        do {
          val p_f = Http(p_req OK as.String)
          val p_res = Await.result(p_f, scala.concurrent.duration.Duration.Inf)
          val p_res_json: JsValue = Json.parse(p_res)
          progressV = (p_res_json \ "results" \ "progress").as[Int]
          report(dtos.ltrid, sender, progressV, "")
          Thread.sleep(1000)
        } while (progressV < 100)
          
        // Save
        val retrieveUrl = dtos.featureExtractUrl + s"?command=download&procId=${procid.toString}&wt=json"
        val r_req = url(retrieveUrl)
        val r_f = Http(r_req OK as.String)
        val r_res = Await.result(r_f, scala.concurrent.duration.Duration.Inf)
        val r_result: FeatureExtractResults = parseResults(Json.parse(r_res), dtos.ltrid)
        sender ! FeatureExtractSetResultMsg(dtos.ltrid, r_result)
    
      }
      
      case Failure(ex) => {
        report(dtos.ltrid, sender, -1, ex.getMessage)
        logger.error(ex.getMessage, ex)
      }
    }
  }
  
  
  
  def parseResults(json: JsValue, ltrid: Int) : FeatureExtractResults = {
    val fnamelist = (json \ "results" \ "result" \ "data" \ "feature").as[Seq[String]]
    val queries =  (json \ "results" \ "result" \ "data" \ "queries").as[List[FeatureExtractResult]]
    var results: List[DocFeature] = List()
    
    queries map {q =>
      q.docs map {d =>
        fnamelist.zipWithIndex.foreach { case (fid,n) =>
          results = results :+ DocFeature(None, n, q.qid, d.id, d.feature(n), ltrid)
        }
      }
    }
    
    FeatureExtractResults(fnamelist, results)
  }
  
}


// DEBUG
/*
object FeatureProgress {
  private var p: Int = 0
  def add(n: Int): Unit = {
    p = p + n
    if(p > 100) p = 100
  }
  def get(): Int = {
    p
  }
  def reset: Unit = {
    p = 0
  }
}
*/

