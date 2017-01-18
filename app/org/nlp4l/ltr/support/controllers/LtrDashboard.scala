/*
 * Copyright 2017 org.NLP4L
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

import scala.concurrent.Await
import scala.util.Failure
import scala.util.Success
import org.nlp4l.ltr.support.dao.DocFeatureDAO
import org.nlp4l.ltr.support.dao.FeatureProgressDAO
import org.nlp4l.ltr.support.dao.LtrannotationDAO
import org.nlp4l.ltr.support.dao.LtrconfigDAO
import org.nlp4l.ltr.support.dao.LtrfeatureDAO
import org.nlp4l.ltr.support.dao.LtrmodelDAO
import org.nlp4l.ltr.support.dao.LtrqueryDAO
import org.nlp4l.ltr.support.models.Ltrconfig
import org.nlp4l.ltr.support.models.Ltrfeature
import org.nlp4l.ltr.support.models.Ltrmodel
import org.nlp4l.ltr.support.models.Ltrquery
import org.nlp4l.ltr.support.models.Menubar
import javax.inject.Inject
import javax.inject.Singleton

import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc.Action
import play.api.mvc.Controller


@Singleton
class LtrDashboard @Inject()(docFeatureDAO: DocFeatureDAO, 
                             ltrfeatureDAO: LtrfeatureDAO, 
                             ltrconfigDAO: LtrconfigDAO,
                             ltrmodelDAO: LtrmodelDAO,
                             ltrqueryDAO: LtrqueryDAO,
                             ltrannotationDAO: LtrannotationDAO,
                             featureProgressDAO: FeatureProgressDAO) extends Controller {

  def index(ltrid: Int) = Action { request =>
    docFeatureDAO.init
    ltrfeatureDAO.init
    ltrconfigDAO.init
    ltrmodelDAO.init
    ltrqueryDAO.init
    ltrannotationDAO.init
    featureProgressDAO.init
    val menubars = buildMenubars(ltrid)
    Ok(org.nlp4l.ltr.support.views.html.dashboard(menubars))
  }

  
  def config(ltrid: Int) = Action {
    val f = ltrconfigDAO.fetchAll()
    val ltrconfigs: Seq[Ltrconfig] = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)
    Ok(org.nlp4l.ltr.support.views.html.config(ltrid,menubars,ltrconfigs))
  }
  
  def editConfig(ltrid: Int, target: Int) = Action {
    val f = ltrconfigDAO.fetchAll()
    val ltrconfigs: Seq[Ltrconfig] = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val ltr = getLtr(target)
    val menubars = buildMenubars(ltrid)
    Ok(org.nlp4l.ltr.support.views.html.editConfig(ltrid,menubars,ltrconfigs,ltr,"",""))
  }
  
  def newConfig(ltrid: Int) = Action {
    val f = ltrconfigDAO.fetchAll()
    val ltrconfigs: Seq[Ltrconfig] = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val ltr = None
    val menubars = buildMenubars(ltrid)
    Ok(org.nlp4l.ltr.support.views.html.editConfig(ltrid,menubars,ltrconfigs,ltr,"",""))
  }
  
  
  def query(ltrid: Int) = Action {
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)
    Ok(org.nlp4l.ltr.support.views.html.query(ltrid,menubars,ltr,"",""))
  }
  
  
  def annotation(ltrid: Int) = Action {
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)
    val f =ltrqueryDAO.fetchNext(ltrid, 0)
    val res = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val ltrquery =res match {
      case Some(x) => Some(x)
      case _ => Some(Ltrquery(Some(0), "", ltrid, false))
    }
    val totalf = ltrqueryDAO.totalCountByLtrid(ltrid)
    val total = Await.result(totalf, scala.concurrent.duration.Duration.Inf)
    val savedf = ltrqueryDAO.totalSavedCountByLtrid(ltrid)
    val saved = Await.result(savedf, scala.concurrent.duration.Duration.Inf)
    Ok(org.nlp4l.ltr.support.views.html.annotation(ltrid,menubars,ltr,ltrquery,(saved,total),"",""))
  }

  def editAnnotation(ltrid: Int, qid: Int) = Action {
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)
    val f = ltrqueryDAO.get(qid)
    Await.ready(f, scala.concurrent.duration.Duration.Inf)
    val ltrquery = f.value.get match {
      case Success(x) => Some(x)
      case Failure(ex) => Some(Ltrquery(Some(0), "", ltrid, false))
    }
    val totalf = ltrqueryDAO.totalCountByLtrid(ltrid)
    val total = Await.result(totalf, scala.concurrent.duration.Duration.Inf)
    val savedf = ltrqueryDAO.totalSavedCountByLtrid(ltrid)
    val saved = Await.result(savedf, scala.concurrent.duration.Duration.Inf)
    Ok(org.nlp4l.ltr.support.views.html.annotation(ltrid,menubars,ltr,ltrquery,(saved,total),"",""))
  }

  def feature(ltrid: Int) = Action {
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)
    Ok(org.nlp4l.ltr.support.views.html.feature(ltrid,menubars,ltr,"",""))
  }

  def training(ltrid: Int) = Action {
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)
    val f = ltrmodelDAO.fetchByLtrid(ltrid)
    val ltrmodels: Seq[Ltrmodel] = Await.result(f, scala.concurrent.duration.Duration.Inf)
    Ok(org.nlp4l.ltr.support.views.html.training(ltrid,menubars,ltr,ltrmodels,"",""))
  }

  def newTraining(ltrid: Int) = Action {
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)
    val f = ltrmodelDAO.fetchByLtrid(ltrid)
    val ltrmodels: Seq[Ltrmodel] = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val ff = ltrfeatureDAO.fetchByLtrid(ltrid)

    val features: Seq[Ltrfeature] = Await.result(ff, scala.concurrent.duration.Duration.Inf)
    Ok(org.nlp4l.ltr.support.views.html.newTraining(ltrid,menubars,ltr,ltrmodels,features,"",""))
  }

  def trainingStatus(ltrid: Int, mid: Int) = Action {
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)
    val f = ltrmodelDAO.fetchByLtrid(ltrid)
    val ltrmodels: Seq[Ltrmodel] = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val ff = ltrfeatureDAO.fetchByLtrid(ltrid)
    val features: Seq[Ltrfeature] = Await.result(ff, scala.concurrent.duration.Duration.Inf)
    val fm = ltrmodelDAO.get(mid)
    val ltrmodel: Ltrmodel = Await.result(fm, scala.concurrent.duration.Duration.Inf)
    val json: JsValue = Json.parse(ltrmodel.feature_list)
    val featureList = (json \ "features").as[Seq[JsValue]].map(_.as[String])
    Ok(org.nlp4l.ltr.support.views.html.trainingStatus(ltrid,menubars,ltr,ltrmodels,featureList,ltrmodel,"",""))
  }

  def dataImport(ltrid: Int) = Action {
    val ltr = getLtr(ltrid)
    val menubars = buildMenubars(ltrid)

    Ok(org.nlp4l.ltr.support.views.html.dataImport(ltrid,menubars,ltr,"",""))
  }

  private def buildMenubars(ltrid: Int): Seq[Menubar] = {
    if(ltrid <= 0) {
      Seq(Menubar("Config","/ltrdashboard/0/config"))
    } else {
      val ltr = getLtr(ltrid)
      val ltrname: String = ltr match {
        case Some(ltr) => "[ " + ltr.name + " ]"
        case None => ""
      }
      Seq(
          Menubar("Config "+ltrname,"/ltrdashboard/" + ltrid + "/config/" + ltrid),
          Menubar("Query","/ltrdashboard/" + ltrid + "/query"),
          Menubar("Annotation","/ltrdashboard/" + ltrid + "/annotation"),
          Menubar("Feature","/ltrdashboard/" + ltrid + "/feature"),
          Menubar("Training","/ltrdashboard/" + ltrid + "/training")
      )
    }
  }
  
  private def getLtr(ltrid: Int): Option[Ltrconfig] = {
      val f = ltrconfigDAO.get(ltrid)
      Await.ready(f, scala.concurrent.duration.Duration.Inf)
      f.value.get match {
        case Success(ltr) => Some(ltr)
        case Failure(ex) => None
      }
  }
  
}
