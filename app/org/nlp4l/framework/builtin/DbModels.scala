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

package org.nlp4l.framework.builtin

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.JsError
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Reads
import play.api.libs.json.Writes
import play.api.mvc.Action
import play.api.mvc.BodyParsers
import play.api.mvc.EssentialAction
import play.api.mvc.Result
import play.api.mvc.Results
import slick.jdbc.GetResult
import org.nlp4l.framework.models.RecordWithAttrbute
import org.nlp4l.framework.models.Dictionary
import org.nlp4l.framework.models.Record
import org.nlp4l.framework.models.Cell

/**
 * Job Table
 */
case class Job(jobId: Option[Int], name: String, config: String, lastRunId: Int, lastRunAt: Option[DateTime], lastDeployAt: Option[DateTime])

/**
 * Job Status Table
 */
case class JobStatus(id: Option[Int], jobId: Int, runId: Int, total: Int, done: Int, message: String = "")


/**
 * Replay Table scheme
 */
case class Replay(runId: Int, hashcode: Int, replay: String, modToHashcode: Int)


object DbModels {
  
  implicit val fWActionResultWrites = new Writes[ActionResult] {
    override def writes(ar: ActionResult): JsValue =
      Json.obj(
        "status" -> ar.status,
        "messages" -> ar.message)
  }
  
  implicit val fWJobWrites = new Writes[Job] {
    override def writes(job: Job): JsValue =
      Json.obj(
        "jobId" -> job.jobId,
        "name" -> job.name,
        "config" -> job.config,
        "lastRunAt" -> job.lastRunAt.map { DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss").print(_) },
        "lastDeployAt" -> job.lastDeployAt.map { DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss").print(_) })
  }

  implicit val fWRecordWrites = new Writes[Record] {
    override def writes(rec: Record): JsValue = {
      val b = Map.newBuilder[String, String]
      rec.cellList foreach { c:Cell =>
        // TODO format関数実行
        if (c.value == null) {
          b += c.name.toLowerCase() -> "" //null
        } else {
          b += c.name.toLowerCase()  -> c.value.toString()
        }
      }
      Json.toJson(b.result())
    }
  }

  implicit val fWRecordWithAttrubuteWrites = new Writes[RecordWithAttrbute] {
    override def writes(rec: RecordWithAttrbute): JsValue = {
      val b = Map.newBuilder[String, String]
      rec.record.cellList foreach { c: Cell =>
        if (c.value == null) {
          b += c.name.toLowerCase()  -> "" //null
        } else {
          var cv: String = c.value.toString()
          rec.attribute.getCellAttribute(c.name.toLowerCase() ) map { ca =>
            cv = ca.format(c.value)
          }
          b += c.name.toLowerCase()  -> cv
        }
      }
      Json.toJson(b.result())
    }
  }

  implicit val fWDictionaryWrites = new Writes[Dictionary] {
    override def writes(dic: Dictionary): JsValue = {
      Json.toJson(dic.recordList)
    }
  }
  
  implicit val fWJobStatusWrites = new Writes[JobStatus] {
    override def writes(js: JobStatus): JsValue = {
      var status: String = if(js.total == js.done) { "Done" } else {"Running" }
      if(js.message != null && !js.message.isEmpty()) status = "Error"
      val jobRun = js.jobId + "/" + js.runId
      Json.obj(
        "status" -> status,
        "jobId" -> js.jobId,
        "runId" -> jobRun,
        "total" -> js.total,
        "done" -> js.done,
        "message" -> js.message)
    }
  }

  implicit val getListStringResult = GetResult[List[String]](prs =>
    (1 to prs.numColumns).map(_ => prs.nextString).toList)
    

  implicit val resultAsStringMap = GetResult[Map[String, String]](prs =>
    (1 to prs.numColumns).map(_ =>
      prs.rs.getMetaData.getColumnName(prs.currentPos + 1).toLowerCase -> prs.nextString).toMap)


  

  def deSerializeRecord(str: String): Record = {
    var in:ObjectInputStream = null
    try {
      in = new ObjectInputStream(new ByteArrayInputStream(str.getBytes("UTF-8")))
      in.readObject().asInstanceOf[Record]
    } finally {
      if(in != null) {
        in.close()
      }
    }
  }
  
  def serializeRecord(rec: Record): String = {
    var b = new ByteArrayOutputStream()
    var out:ObjectOutputStream = null
    try {
      out = new ObjectOutputStream(b)
      out.writeObject(rec)
    } finally {
      if(out != null) {
        out.close()
      }
      b.close()
    }
    b.toString()
  }

}


object Actions extends Results with BodyParsers {

  def JsonAction[A](action: A => Result)(implicit reader: Reads[A]): EssentialAction = {
    Action(parse.json) { implicit request =>
      request.body.validate[A].fold(
        valid = { json =>
          action(json)
        },
        invalid = { e =>
          BadRequest(JsError.toJson(e))
        }
      )
    }
  }
  
}


/**
 * Message class between actors related job execution
 */
case class JobMessage(jobId: Int)

/**
 * Response of action
 */
case class ActionResult(status:Boolean, message:Seq[String])



