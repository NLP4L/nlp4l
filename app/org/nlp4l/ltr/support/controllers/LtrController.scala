/*
 * Copyright 2015-2017 org.NLP4L
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

package org.nlp4l.ltr.support.controllers

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.UUID
import java.util.concurrent.TimeUnit

import scala.collection.JavaConversions.asScalaBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import org.nlp4l.ltr.support.actors._
import org.nlp4l.ltr.support.dao.DocFeatureDAO
import org.nlp4l.ltr.support.dao.LtrfeatureDAO
import org.nlp4l.ltr.support.dao.LtrannotationDAO
import org.nlp4l.ltr.support.dao.LtrconfigDAO
import org.nlp4l.ltr.support.dao.LtrmodelDAO
import org.nlp4l.ltr.support.dao.LtrqueryDAO
import org.nlp4l.ltr.support.models._
import org.nlp4l.ltr.support.models.ViewModels._
import org.nlp4l.ltr.support.models.DbModels._
import org.nlp4l.ltr.support.models.ClickModels._
import com.google.inject.name.Named
import akka.actor.ActorRef
import akka.pattern.AskableActorRef
import akka.util.Timeout
import javax.inject.Inject

import com.typesafe.config.{Config, ConfigFactory}
import org.joda.time.DateTime
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.Action
import play.api.mvc.Controller
import org.nlp4l.ltr.support.dao.FeatureProgressDAO
import org.nlp4l.ltr.support.procs.{ClickModelAnalyzer, DeployerFactory}
import play.api.Logger

import scala.io.Source

class LtrController @Inject()(docFeatureDAO: DocFeatureDAO, 
                             ltrfeatureDAO: LtrfeatureDAO, 
                             ltrconfigDAO: LtrconfigDAO,
                             ltrmodelDAO: LtrmodelDAO,
                             ltrqueryDAO: LtrqueryDAO,
                             ltrannotationDAO: LtrannotationDAO,
                             featureProgressDAO: FeatureProgressDAO,
                             @Named("progress-actor") progressActor: ActorRef ) extends Controller {


  implicit val timeout = Timeout(5000, TimeUnit.MILLISECONDS)
  val pa = new AskableActorRef(progressActor)

  private val logger = Logger(this.getClass)

  def saveLtrConfig(ltrid: Int) = Action.async(parse.json) { request =>
    val data = request.body
    val name = (data \ "name").as[String]
    val annotationType = (data \ "annotationType").as[String]
    val trainerFactoryClassName = (data \ "trainerFactoryClassName").as[String]
    val trainerFactoryClassSettings = (data \ "trainerFactoryClassSettings").as[String]
    val deployerFactoryClassName = (data \ "deployerFactoryClassName").as[String]
    val deployerFactoryClassSettings = (data \ "deployerFactoryClassSettings").as[String]
    val searchUrl = (data \ "searchUrl").as[String]
    val featureExtractUrl = (data \ "featureExtractUrl").as[String]
    val featureExtractConfig = (data \ "featureExtractConfig").as[String]
    val docUniqField = (data \ "docUniqField").as[String]
    val docTitleField = (data \ "docTitleField").as[String]
    val docBodyField = (data \ "docBodyField").as[String]
    val labelMax = (data \ "labelMax").as[String]

    if (name.isEmpty) {
      Future.successful(BadRequest("Name cannot be empty."))
    } else {
      val newLtr: Ltrconfig = Ltrconfig(Some(ltrid), name, annotationType, trainerFactoryClassName, Some(trainerFactoryClassSettings), deployerFactoryClassName, Some(deployerFactoryClassSettings), searchUrl, featureExtractUrl, featureExtractConfig, docUniqField, docTitleField, docBodyField, labelMax.toInt)
      val f: Future[Ltrconfig] = ltrconfigDAO.get(ltrid)
      Await.ready(f, scala.concurrent.duration.Duration.Inf)
      f.value.get match {
        case Success(ltr) => {
          ltrconfigDAO.update(newLtr) map {
            res => {
              val jsonResponse = Json.toJson(newLtr)
              Ok(jsonResponse)
            }
          } recover {
            case e => InternalServerError("Add failed. " + e.getMessage)
          }
        }
        case Failure(ex) => {
          ltrconfigDAO.insert(newLtr) map {
            res => {
              val jsonResponse = Json.toJson(res)
              Ok(jsonResponse)
            }
          } recover {
            case e => InternalServerError("Add failed. " + e.getMessage)
          }
        }
      }

    }
  }

  def deleteLtrConfig(ltrid: Int) = Action.async {
    val modelf = ltrmodelDAO.deleteByLtrid(ltrid)
    Await.result(modelf, scala.concurrent.duration.Duration.Inf)
    val docff = docFeatureDAO.deleteByLtrid(ltrid)
    Await.result(docff, scala.concurrent.duration.Duration.Inf)
    val fdelf = ltrfeatureDAO.deleteByLtrid(ltrid)
    Await.result(fdelf, scala.concurrent.duration.Duration.Inf)
    val delaf = ltrannotationDAO.deleteByLtrid(ltrid)
    Await.ready(delaf, scala.concurrent.duration.Duration.Inf)
    val fpf = featureProgressDAO.deleteByLtrid(ltrid)
    Await.result(fpf, scala.concurrent.duration.Duration.Inf)
    val delqf = ltrqueryDAO.deleteByLtrid(ltrid)
    Await.ready(delqf, scala.concurrent.duration.Duration.Inf)
    val f: Future[Ltrconfig] = ltrconfigDAO.get(ltrid)
    val ltr = Await.result(f, scala.concurrent.duration.Duration.Inf)
    ltrconfigDAO.delete(ltrid) map {
      case (a) => {
        Ok(Json.toJson(ActionResult(true, Seq("success"))))
      }
    } recover {
      case e => InternalServerError("Delete failed. " + e.getMessage)
    }
  }


  def saveQuery(ltrid: Int) = Action(parse.multipartFormData) { request =>
    request.body.file("query") map { file =>
      val uuid = UUID.randomUUID().toString
      val temp = new File(s"/tmp/$uuid")
      file.ref.moveTo(temp, replace = true)
      val tempPath = Paths.get(temp.getAbsolutePath)
      val lines = Files.readAllLines(tempPath).toList
      lines.foreach(l => {
        if (!l.trim().isEmpty) {
          val ltrquery = Ltrquery(None, l, ltrid, false)
          val f = ltrqueryDAO.insert(ltrquery)
          Await.ready(f, scala.concurrent.duration.Duration.Inf)
        }
      })
    }
    Redirect("/ltrdashboard/" + ltrid + "/query")
  }

  def importData(ltrid: Int) = Action(parse.multipartFormData) { request =>
    val result = request.body.file("importFile") map { file =>
      val uuid = UUID.randomUUID().toString
      val temp = new File(s"/tmp/$uuid")
      file.ref.moveTo(temp, replace = true)
      val importType = request.body.dataParts.getOrElse("importType", Seq("QAD")).head
      val importSettings = request.body.dataParts.getOrElse("importSettings", Seq("")).head
      if (importType.equals("ICM") || importType.equals("DCM")) {
        val source: String = Source.fromFile(temp.getAbsolutePath).getLines.mkString
        val json: JsValue = Json.parse(source)
        val data =  (json \ "data").as[Seq[ImpressionLog]]
        val dcm = true
        val config = ConfigFactory.parseString(importSettings)
        val clickModel = new ClickModelAnalyzer()
        val dataTopN = if (config.hasPath("topQueries")) clickModel.filterTopQueries(data, config.getInt("topQueries")) else data
        val dataFiltered = if (importType.equals("DCM")) clickModel.filterAsDCM(dataTopN) else dataTopN
        val clickRates = clickModel.calcClickRate(dataFiltered)
        val relevanceDegreeMapping = config.getDoubleList("relevanceDegreeMapping").map(_.toFloat).toArray
        val clickRateLabels: Map[String, Map[String, Int]] = clickModel.convertClickRateToLabel(clickRates, relevanceDegreeMapping)
        clickRateLabels.foreach(qg => {
          val ltrquery = Ltrquery(None, qg._1, ltrid, true)
          val f = ltrqueryDAO.insert(ltrquery)
          val query = Await.result(f, scala.concurrent.duration.Duration.Inf)
          val list = qg._2.map(annotation => {
            Ltrannotation(query.qid.get, annotation._1, annotation._2, ltrid)
          }).toList
          ltrannotationDAO.insertList(list)
        })
        clickRateLabels.size + " queries as " + importType + " imported."
      } else {
        val tempPath = Paths.get(temp.getAbsolutePath)
        val lines = Files.readAllLines(tempPath).toList.filter(!_.trim().isEmpty)
        val qgroup: Map[String, Seq[Array[String]]] = lines.map(_.split(",")).groupBy(_ (0))
        qgroup.foreach(qg => {
          // csv format: easy validation here.
          val ltrquery = Ltrquery(None, qg._2.head(1), ltrid, true)
          val list = qg._2.map(line => {
            Ltrannotation(0, line(2), line(3).toInt, ltrid)
          })
        })
        qgroup.foreach(qg => {
          val ltrquery = Ltrquery(None, qg._2.head(1), ltrid, true)
          val f = ltrqueryDAO.insert(ltrquery)
          val query = Await.result(f, scala.concurrent.duration.Duration.Inf)
          val list = qg._2.map(line => {
            Ltrannotation(query.qid.get, line(2), line(3).toInt, ltrid)
          })
          ltrannotationDAO.insertList(list)
        })
        qgroup.size + " queries with annotations imported."
     }
    }
    val jsonResponse = Json.obj(
      "status" -> true,
      "msg" -> ("Import Success: " + result.get)
    )
    Ok(jsonResponse)
  }

  def clearAllAnnotation(ltrid: Int) = Action { request =>
    val clearf = ltrqueryDAO.clearCheckedFlg(ltrid)
    Await.ready(clearf, scala.concurrent.duration.Duration.Inf)
    val delf = ltrannotationDAO.deleteByLtrid(ltrid)
    Await.ready(delf, scala.concurrent.duration.Duration.Inf)
    Redirect("/ltrdashboard/" + ltrid + "/query")
  }

  def clearAnnotation(ltrid: Int, qid : Int) = Action { request =>
    val delfa = ltrannotationDAO.deleteByQid(qid)
    Await.ready(delfa, scala.concurrent.duration.Duration.Inf)
    val fq: Future[Ltrquery] = ltrqueryDAO.get(qid)
    val ltrquery = Await.result(fq, scala.concurrent.duration.Duration.Inf)
    val fu: Future[Int] = ltrqueryDAO.update(ltrquery.copy(checked_flg = false))
    Await.ready(fu, scala.concurrent.duration.Duration.Inf)
    Ok(Json.toJson(ActionResult(true, Seq("cleared"))))
  }

  def listQuery(ltrid: Int) = Action { request =>
    val offset = request.getQueryString("offset") match {
      case Some(x) if x != "" => x.toInt
      case _ => 0
    }
    val size = request.getQueryString("limit") match {
      case Some(x) => x.toInt
      case _ => 10
    }
    val sort = request.getQueryString("sort") match {
      case Some(c) => c
      case _ => "qid"
    }
    val order = request.getQueryString("order") match {
      case Some(c) => c
      case _ => "asc"
    }
    val totalf = ltrqueryDAO.totalCountByLtrid(ltrid)
    val total = Await.result(totalf, scala.concurrent.duration.Duration.Inf)
    val qlistf = ltrqueryDAO.fetchByLtrid(ltrid, "qid", "asc", offset, size)
    val qlist = Await.result(qlistf, scala.concurrent.duration.Duration.Inf)
    val jsonResponse = Json.obj(
      "total" -> total,
      "rows" -> Json.toJson(qlist)
    )
    Ok(jsonResponse)
  }

  def nextQuery(ltrid: Int, qid : Int) = Action {
    val f =ltrqueryDAO.fetchNext(ltrid, qid)
    val res = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val ltrquery = res match {
      case Some(x) => x
      case _ => Ltrquery(Some(0), "", ltrid, false)
    }
    Redirect("/ltrdashboard/" + ltrid + "/annotation/" + ltrquery.qid.get)
  }

  def deleteQuery(ltrid: Int, qid : Int) = Action.async {
    val delfa = ltrannotationDAO.deleteByQid(qid)
    Await.ready(delfa, scala.concurrent.duration.Duration.Inf)
    val f: Future[Int] = ltrqueryDAO.delete(qid)
    Await.ready(f, scala.concurrent.duration.Duration.Inf)
    Future.successful(Ok(Json.toJson(ActionResult(true, Seq("success")))))
  }

  def search(ltrid: Int, qid: Int) = Action { request =>
    val fc: Future[Ltrconfig] = ltrconfigDAO.get(ltrid)
    val ltrconfig = Await.result(fc, scala.concurrent.duration.Duration.Inf)

    val solrSearch = new SolrSearch(ltrconfig.searchUrl)
    val queryStr = request.getQueryString("q") match {
      case Some(q) => q
      case _ => {
        val fq: Future[Ltrquery] = ltrqueryDAO.get(qid)
        val ltrquery = Await.result(fq, scala.concurrent.duration.Duration.Inf)
        ltrquery.query
      }
    }
    val solrRes = solrSearch.search(queryStr)
    if (solrRes.statusSuccess) {
      val fl = ltrannotationDAO.getByQid(qid)
      val currentLabels = Await.result(fl, scala.concurrent.duration.Duration.Inf)
      val labelMap: Map[String, Int] = currentLabels.map(l => l.docid -> l.label).toMap
      val idField = ltrconfig.docUniqField
      val titleField = ltrconfig.docTitleField
      val bodyField = ltrconfig.docBodyField
      val docList = solrRes.docs.map(doc => {
        val idText = solrRes.getFirstValueAsString(doc, idField)
        val titleTextHL = solrRes.getHighlighting(idText,titleField)
        val titleText =
          if (titleTextHL != null) titleTextHL
          else {
            val titleText = solrRes.getValueAsString(doc, titleField, "...")
            if (titleText == null) ""
            else titleText
          }
        val bodyTextHL = solrRes.getHighlighting(idText,bodyField)
        val bodyText =
          if (bodyTextHL != null) bodyTextHL
          else {
            val bodyText = solrRes.getValueAsString(doc, bodyField, "...")
            if (bodyText == null) ""
            else if (bodyText.length > 300) bodyText.take(300) + "..."
            else bodyText
          }
        Json.obj(
          "id" -> idText,
          "label" -> Json.toJson(labelMap.getOrElse(idText, 0)),
          "title" -> titleText,
          "body" -> bodyText
        )
      })
      val jsonResponse = Json.obj(
        "status" -> 0,
        "rows" -> docList
      )
      Ok(jsonResponse)
    } else {
      val jsonResponse = Json.obj(
        "status" -> solrRes.status,
        "msg" -> solrRes.errorMsg
      )
      Ok(jsonResponse)
    }
  }

  def searchById(ltrid: Int) = Action { request =>
    val fc: Future[Ltrconfig] = ltrconfigDAO.get(ltrid)
    val ltrconfig = Await.result(fc, scala.concurrent.duration.Duration.Inf)

    val solrSearch = new SolrSearch(ltrconfig.searchUrl)
    val queryStr = ltrconfig.docUniqField + ":\"" + request.getQueryString("id").get + "\""
    val solrRes = solrSearch.search(queryStr)
    val idField = ltrconfig.docUniqField
    val titleField = ltrconfig.docTitleField
    val bodyField = ltrconfig.docBodyField
    val doc = solrRes.docs.head
    val jsonResponse = doc
    Ok(jsonResponse)
  }

  def saveLabels(ltrid: Int, qid: Int) = Action(parse.json) { request =>
    val data = request.body
    val query = (data \ "query").as[String]
    val labels = (data \ "labels").as[Seq[JsObject]].map( obj =>
      (obj.value.get("docId").get.as[String], obj.value.get("label").get.as[Int])
    )
    val saveQid = if (qid == 0) {
      val ltrquery = Ltrquery(None, query, ltrid, false)
      val f = ltrqueryDAO.insert(ltrquery)
      val newQuery = Await.result(f, scala.concurrent.duration.Duration.Inf)
      newQuery.qid.get
    } else {
      qid
    }
    val delfa = ltrannotationDAO.deleteByQid(saveQid)
    Await.ready(delfa, scala.concurrent.duration.Duration.Inf)
    val list = labels.map(label => {
      Ltrannotation(saveQid, label._1, label._2, ltrid)
    })
    ltrannotationDAO.insertList(list)
    val fq: Future[Ltrquery] = ltrqueryDAO.get(saveQid)
    val ltrquery = Await.result(fq, scala.concurrent.duration.Duration.Inf)
    val fu: Future[Int] = ltrqueryDAO.update(ltrquery.copy(checked_flg = true))
    Await.ready(fu, scala.concurrent.duration.Duration.Inf)
    val totalf = ltrqueryDAO.totalCountByLtrid(ltrid)
    val total = Await.result(totalf, scala.concurrent.duration.Duration.Inf)
    val savedf = ltrqueryDAO.totalSavedCountByLtrid(ltrid)
    val saved = Await.result(savedf, scala.concurrent.duration.Duration.Inf)
    val jsonResponse = Json.obj(
      "qid" -> saveQid,
      "saved" -> saved,
      "total" -> total
    )
    Ok(jsonResponse)
  }

  def startFeatureEtraction(ltrid: Int) = Action {
    val f: Future[Ltrconfig] = ltrconfigDAO.get(ltrid)
    val ltr = Await.result(f, scala.concurrent.duration.Duration.Inf)

    val totalf = ltrqueryDAO.totalCountByLtrid(ltrid)
    val total = Await.result(totalf, scala.concurrent.duration.Duration.Inf)
    val qlistf = ltrqueryDAO.fetchByLtrid(ltrid, "qid", "asc", 0, total)
    val qlist = Await.result(qlistf, scala.concurrent.duration.Duration.Inf)

    var dtos: List[FeatureExtractDTO] = List()
    qlist.map { q =>
      val docsf = ltrannotationDAO.getByQid(q.qid.getOrElse(0))
      val docs = Await.result(docsf, scala.concurrent.duration.Duration.Inf)
      val dto: FeatureExtractDTO = FeatureExtractDTO(q.qid.getOrElse(0), q.query, docs.map(_.docid).toList)
      dtos = dtos :+ dto
    }
    val fedtos = FeatureExtractDTOs(ltr.ltrid.getOrElse(0), ltr.featureExtractUrl, ltr.featureExtractConfig, ltr.docUniqField, dtos)
    
    progressActor ! FeatureExtractStartMsg(fedtos)
    Ok(Json.toJson(ActionResult(true, Seq("started"))))
  }
  
  def getFeatureProgress(ltrid: Int) = Action.async {
    val f = pa ? FeatureExtractGetProgressMsg(ltrid)
    f.map(result => Ok(result.toString()))
  }

  def getFeatureProgressMessage(ltrid: Int) = Action.async {
    val f = pa ? FeatureExtractGetProgressMessageMsg(ltrid)
    f.map(result => Ok(result.toString()))
  }
  
  def clearFeatureProgress(ltrid: Int) = Action {
    progressActor ! FeatureExtractClearResultMsg(ltrid)
    Ok(Json.toJson(ActionResult(true, Seq("cleared"))))
  }

  def startTraining(ltrid: Int) = Action { request =>
    val f: Future[Ltrconfig] = ltrconfigDAO.get(ltrid)
    val ltr = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val runid = ltrmodelDAO.nextRunId(ltrid)
    val feature_list = request.getQueryString("features").get

    val ff = ltrfeatureDAO.fetchByLtrid(ltrid)
    val featureList: Seq[Ltrfeature] = Await.result(ff, scala.concurrent.duration.Duration.Inf).sortBy(_.fid)
    val selected: Array[Int] = feature_list.split(",").map(f => f.toInt)
    val selectedFeatureList = featureList.filter(f => selected.contains(f.fid.get))

    val modelFeatureList = Json.prettyPrint(Json.obj("features" -> selectedFeatureList.map(_.name)))
    val ltrmodel: Ltrmodel = Ltrmodel(None,ltrid,runid,modelFeatureList,None,0,0,"",Some(new DateTime()),None)
    val fm: Future[Ltrmodel] = ltrmodelDAO.insert(ltrmodel)
    val newModel = Await.result(fm, scala.concurrent.duration.Duration.Inf)
    val fd = docFeatureDAO.fetchByFids(ltrid, selected)
    val docFeatures: Seq[DocFeature] = Await.result(fd, scala.concurrent.duration.Duration.Inf)

    val fl = ltrannotationDAO.fetchByLtrid(ltrid)
    val ltrannotations = Await.result(fl, scala.concurrent.duration.Duration.Inf)

    val trainingRequest = TrainingRequest(ltrid, runid, ltr, selectedFeatureList, docFeatures, ltrannotations)

    progressActor ! TrainingStartMsg(trainingRequest)

    val jsonResponse = Json.obj(
      "mid" -> newModel.mid.get
    )
    Ok(jsonResponse)
  }

  def getTrainingProgress(ltrid: Int, runid: Int) = Action.async {
    val f = pa ? TrainingGetProgressMsg(ltrid, runid)
    f.map(result => Ok(result.toString()))
  }

  def deleteTraining(ltrid: Int, mid : Int) = Action.async {
    val fd: Future[Int] = ltrmodelDAO.delete(mid)
    Await.ready(fd, scala.concurrent.duration.Duration.Inf)
    Future.successful(Ok(Json.toJson(ActionResult(true, Seq("success")))))
  }

  def deployModel(ltrid: Int, mid : Int) = Action {
    val f: Future[Ltrconfig] = ltrconfigDAO.get(ltrid)
    val ltrconfig = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val fm = ltrmodelDAO.get(mid)
    val ltrmodel: Ltrmodel = Await.result(fm, scala.concurrent.duration.Duration.Inf)
    try {
      val settings = ltrconfig.deployerFactoryClassSettings
      val config = if (settings.isDefined) ConfigFactory.parseString(settings.get) else ConfigFactory.empty()
      val constructor = Class.forName(ltrconfig.deployerFactoryClassName).getConstructor(classOf[Config])
      val factory = constructor.newInstance(config).asInstanceOf[DeployerFactory]
      val deployer = factory.getInstance()
      deployer.deploy(ltrmodel.model_data.get)
      Ok(Json.toJson(ActionResult(true, Seq("deployed."))))
    } catch {
      case e: Exception =>
        logger.error(e.getMessage, e)
        Ok(Json.toJson(ActionResult(false, Seq(e.getMessage))))
    }
  }
}



