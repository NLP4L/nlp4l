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

import play.api.libs.json.Writes
import play.api.libs.json.JsValue
import play.api.libs.json.Json

/**
 * MenuBar
 */
case class Menubar (name: String, url: String)

/**
 * Response of action
 */
case class ActionResult(status:Boolean, message:Seq[String])

object ViewModels {
  
  implicit val fWActionResultWrites = new Writes[ActionResult] {
    override def writes(ar: ActionResult): JsValue =
      Json.obj(
        "status" -> ar.status,
        "messages" -> ar.message)
  }
  
}