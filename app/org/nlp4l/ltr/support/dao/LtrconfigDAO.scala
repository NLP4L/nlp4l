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

/**
 * Job Dao
 */
package org.nlp4l.ltr.support.dao

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import javax.inject.Inject
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.jdbc.meta.MColumn
import slick.jdbc.meta.MTable
import slick.lifted.ProvenShape.proveShapeOf
import com.github.tototoshi.slick.H2JodaSupport.datetimeTypeMapper
import org.joda.time.DateTime

import org.nlp4l.ltr.support.models.DbModels
import org.nlp4l.ltr.support.models.Ltrconfig



class LtrconfigDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] {
  import driver.api._

  private val logger = Logger(this.getClass)
  
  class LtrconfigTable(tag: Tag) extends Table[Ltrconfig](tag, "LTRCONFIGS") {
    def ltrid = column[Int]("LTRID", O.PrimaryKey, O.AutoInc)
    def name = column[String]("NAME")
    def annotation_type = column[String]("ANNOTATION_TYPE")
    def trainer_factory_class_name = column[String]("TRAINER_FACTORY_CLASS_NAME")
    def trainer_factory_class_settings = column[Option[String]]("TRAINER_FACTORY_CLASS_SETTINGS")
    def deployer_factory_class_name = column[String]("DEPLOYER_FACTORY_CLASS_NAME")
    def deployer_factory_class_settings = column[Option[String]]("DEPLOYER_FACTORY_CLASS_SETTINGS")
    def search_url = column[String]("SEARCH_URL")
    def feature_extract_url = column[String]("FEATURE_EXTRACT_URL")
    def feature_extract_config = column[String]("FEATURE_EXTRACT_CONFIG")
    def doc_uniq_field = column[String]("DOC_UNIQ_FIELD")
    def doc_title_field = column[String]("DOC_TITLE_FIELD")
    def doc_body_field = column[String]("DOC_BODY_FIELD")
    def label_max = column[Int]("LABEL_MAX")
    def * = (ltrid.?, name, annotation_type, trainer_factory_class_name, trainer_factory_class_settings, deployer_factory_class_name, deployer_factory_class_settings, search_url, feature_extract_url, feature_extract_config, doc_uniq_field, doc_title_field, doc_body_field, label_max) <> (Ltrconfig.tupled, Ltrconfig.unapply)
  }

  val ltrconfigs = TableQuery[LtrconfigTable]

  def init = db.run(ltrconfigs.schema.create)

  def fetchAll(): Future[Seq[Ltrconfig]] = db.run(ltrconfigs.result)
  
  def count(): Int = ltrconfigs.length.asInstanceOf[Int]

  def get(ltrid: Int): Future[Ltrconfig] = {
    val query = ltrconfigs.filter(_.ltrid === ltrid)
    db.run(query.result.head)
  }

  def insert(ltrconfig: Ltrconfig): Future[Ltrconfig] = {
    val ltrWithId = (ltrconfigs returning ltrconfigs.map(_.ltrid) into ((ltrconfig, id) => ltrconfig.copy(ltrid=Some(id)))) += ltrconfig
    db.run(ltrWithId)
  }

  def update(ltrconfig: Ltrconfig): Future[Int] = {
    val query = ltrconfigs.filter(_.ltrid === ltrconfig.ltrid)
    db.run(query.update(ltrconfig))
  }

  def delete(ltrid: Int): Future[Int] = {
    val query = ltrconfigs.filter(_.ltrid === ltrid)
    val res = db.run(query.delete)
    res
  }
  
  def fetch(sort: String, order: String, offset: Int = 0, size: Int = 10): Future[Seq[Ltrconfig]] = {
    sort match {
      case "ltrid" =>
        order match {
          case "asc" =>
            db.run(ltrconfigs.sortBy(_.ltrid.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(ltrconfigs.sortBy(_.ltrid.desc).drop(offset).take(size).result)
        }
    }
  }

  
  

}
