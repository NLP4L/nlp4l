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

import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Writes

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat


  
/**
 * Ltrconfig Table
 */
case class Ltrconfig (
    ltrid: Option[Int], 
    name: String, 
    annotationType: String,
    trainerFactoryClassName: String,
    trainerFactoryClassSettings: Option[String],
    deployerFactoryClassName: String,
    deployerFactoryClassSettings: Option[String],
    searchUrl: String,
    featureExtractUrl: String,
    featureExtractConfig: String,
    docUniqField: String,
    docTitleField: String,
    docBodyField: String,
    labelMax: Int
)

/**
 * ltrfeatures Table
 */
case class Ltrfeature (
    fid: Option[Int],
    ltrid: Int,
    name: String
)

/**
 * featureprogress Table
 */
case class FeatureProgress (
    id: Option[Int],
    ltrid: Int,
    progress: Int,
    message: String
)

/**
 * ltrqueries Table
 */
case class Ltrquery (
    qid: Option[Int],
    query: String,
    ltrid: Int,
    checked_flg: Boolean
)

/**
 * doc_features Table
 */
case class DocFeature (
    id: Option[Int],
    fid: Int,
    qid: Int,
    docid: String,
    value: Float,
    ltrid: Int
)



/**
 * ltrmodes Table
 */
case class Ltrmodel (
    mid: Option[Int],
    ltrid: Int,
    runid: Int,
    feature_list: String,
    model_data: Option[String],
    status: Int,
    progress: Int,
    message: String,
    started_at: Option[DateTime],
    finished_at: Option[DateTime]
)


/**
 * ltrannotation Table
 */
case class Ltrannotation (
    qid: Int,
    docid: String,
    label: Int,
    ltrid: Int
)



object DbModels {
  
  implicit val fWLtrconfigWrites = new Writes[Ltrconfig] {
    override def writes(d: Ltrconfig): JsValue =
      Json.obj(
        "ltrid" -> d.ltrid,
        "name" -> d.name,
        "annotationType" -> d.annotationType,
        "trainerFactoryClassName" -> d.trainerFactoryClassName,
        "trainerFactoryClassSettings" -> d.trainerFactoryClassSettings,
        "deployerFactoryClassName" -> d.deployerFactoryClassName,
        "deployerFactoryClassSettings" -> d.deployerFactoryClassSettings,
        "searchUrl" -> d.searchUrl,
        "featureExtractUrl" -> d.featureExtractUrl,
        "featureExtractUrl" -> d.featureExtractConfig,
        "docUniqField" -> d.docUniqField,
        "docTitleField" -> d.docTitleField,
        "docBodyField" -> d.docBodyField,
        "labelMax" -> d.labelMax
      )
  }
  
  implicit val fWLtrfeatureWrites = new Writes[Ltrfeature] {
    override def writes(d: Ltrfeature): JsValue =
      Json.obj(
        "fid" -> d.fid,
        "ltrid" -> d.ltrid,
        "name" -> d.name
      )
  }

  implicit val fWLtrqueryWrites = new Writes[Ltrquery] {
    override def writes(d: Ltrquery): JsValue =
      Json.obj(
        "qid" -> d.qid,
        "query" -> d.query,
        "ltrid" -> d.ltrid,
        "checked_flg" -> d.checked_flg
      )
  }
  
  implicit val fWDocFeatureWrites = new Writes[DocFeature] {
    override def writes(d: DocFeature): JsValue =
      Json.obj(
        "qid" -> d.qid,
        "docid" -> d.docid,
        "qid" -> d.qid,
        "value" -> d.value,
        "ltrid" -> d.ltrid
      )
  } 
  
  implicit val fWLtrmodelWrites = new Writes[Ltrmodel] {
    override def writes(d: Ltrmodel): JsValue =
      Json.obj(
        "mid" -> d.mid,
        "ltrid" -> d.ltrid,
        "runid" -> d.runid,
        "feature_list" -> d.feature_list,
        "model_data" -> d.model_data,
        "status" -> d.status,
        "progress" -> d.progress,
        "started_at" -> d.started_at.map { DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss").print(_) },
        "finied_at" -> d.finished_at.map { DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss").print(_) }
      )
  }  
  
}
