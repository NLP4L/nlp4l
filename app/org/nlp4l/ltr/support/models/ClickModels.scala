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

package org.nlp4l.ltr.support.models

import play.api.libs.json._
import play.api.libs.functional.syntax._


/**
 * Click Model Log Data
 */
case class ImpressionLog(
  query: String,
  impressions: List[String],
  clicks: List[String]
)

object ClickModels {

  implicit val fWImpressionLogWrites = new Writes[ImpressionLog] {
    override def writes(d: ImpressionLog): JsValue =
      Json.obj(
        "query" -> d.query,
        "impressions" -> d.impressions,
        "clicks" -> d.clicks)
  }
  implicit val fWImpressionLogReads: Reads[ImpressionLog] = (
    (JsPath \ "query").read[String] and
    (JsPath \ "impressions").read[List[String]] and
    (JsPath \ "clicks").read[List[String]])(ImpressionLog.apply _)
}