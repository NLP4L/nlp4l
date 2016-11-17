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

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json._
import play.api.libs.json.JsPath
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Reads
import play.api.libs.json.Writes


/**
 * Basic dto class for feature extraction
 */
case class FeatureExtractDTO(
  qid: Int,
  query: String,
  docs: List[String])

case class FeatureExtractDTOs(
  ltrid: Int,
  featureExtractUrl: String,
  featureExtractConfig: String,
  idField: String,
  dtos: List[FeatureExtractDTO]
)

case class FeatureExtractQueries(
  idField: String,
  queries: List[FeatureExtractDTO]
)

case class FeatureExtractParameter(
  data: FeatureExtractQueries
)

case class FeatureExtractResultDoc (
    feature: Seq[Float],
    id: String
)
case class FeatureExtractResult (
    qid: Int,
    query: String,
    docs: List[FeatureExtractResultDoc]
)




case class FeatureExtractResults (
  feature: Seq[String],
  results: List[DocFeature]
)

object LtrModels {

  implicit val fWFeatureExtractDTOWrites = new Writes[FeatureExtractDTO] {
    override def writes(d: FeatureExtractDTO): JsValue =
      Json.obj(
        "qid" -> d.qid,
        "query" -> d.query,
        "docs" -> d.docs)
  }

  implicit val fWFeatureExtractDTOsWrites = new Writes[FeatureExtractDTOs] {
    override def writes(d: FeatureExtractDTOs): JsValue =
      Json.obj(
        "queries" -> d.dtos)
  }
  
  implicit val fWFeatureExtractQueriesWrites = new Writes[FeatureExtractQueries] {
    override def writes(d: FeatureExtractQueries): JsValue =
      Json.obj(
        "idField" -> d.idField,
        "queries" -> d.queries)
  }
  
  implicit val fWFeatureExtractParameterWrites = new Writes[FeatureExtractParameter] {
    override def writes(d: FeatureExtractParameter): JsValue =
      Json.obj(
        "data" -> d.data)
  }
  
  
  implicit val fWFeatureExtractResultDocReads: Reads[FeatureExtractResultDoc] = (
    (JsPath \ "feature").read[Seq[Float]] and
    (JsPath \ "id").read[String])(FeatureExtractResultDoc.apply _)

  implicit val fWFeatureExtractResultReads: Reads[FeatureExtractResult] = (
    (JsPath \ "qid").read[Int] and
    (JsPath \ "query").read[String] and
    (JsPath \ "docs").read[List[FeatureExtractResultDoc]])(FeatureExtractResult.apply _)
  
//  implicit val fWFeatureExtractResultsReads: Reads[FeatureExtractResults] = (
//    (JsPath \ "queries").read[List[FeatureExtractResult]])(FeatureExtractResults.apply _)


  implicit val fWFeatureExtractResultDocWrites = new Writes[FeatureExtractResultDoc] {
    override def writes(d: FeatureExtractResultDoc): JsValue =
      Json.obj(
        "id" -> d.id,
        "feature" -> d.feature)
  }
  
  implicit val fWFeatureExtractResultWrites = new Writes[FeatureExtractResult] {
    override def writes(d: FeatureExtractResult): JsValue =
      Json.obj(
        "qid" -> d.qid,
        "query" -> d.query,
        "docs" -> d.docs)
  }
  
//  implicit val fWFeatureExtractResultsWrites = new Writes[FeatureExtractResults] {
//    override def writes(d: FeatureExtractResults): JsValue =
//      Json.obj(
//        "queries" -> d.queries)
//  }

}


