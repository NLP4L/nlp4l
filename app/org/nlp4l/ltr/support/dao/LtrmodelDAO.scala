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

import scala.concurrent.{Await, Future}
import org.nlp4l.ltr.support.models.Ltrmodel
import javax.inject.Inject

import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.lifted.ProvenShape.proveShapeOf
import com.github.tototoshi.slick.H2JodaSupport.datetimeTypeMapper
import org.joda.time.DateTime


class LtrmodelDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] {
  import driver.api._

  private val logger = Logger(this.getClass)
  
  class LtrmodelTable(tag: Tag) extends Table[Ltrmodel](tag, "LTRMODELS") {
    def mid = column[Int]("MID", O.PrimaryKey, O.AutoInc)
    def ltrid = column[Int]("LTRID")
    def runid = column[Int]("RUNID")
    def feature_list = column[String]("FEATURE_LIST")
    def model_data = column[Option[String]]("MODEL_DATA")
    def status = column[Int]("STATUS")
    def progress = column[Int]("PROGRESS")
    def message = column[String]("MESSAGE")
    def started_at = column[Option[DateTime]]("STARTED_AT")
    def finished_at = column[Option[DateTime]]("FINISHED_AT")
    def * = (mid.?, ltrid, runid, feature_list, model_data, status, progress, message, started_at, finished_at) <> (Ltrmodel.tupled, Ltrmodel.unapply)
  }

  val ltrmodels = TableQuery[LtrmodelTable]

  def init = db.run(ltrmodels.schema.create)

  def fetchAll(): Future[Seq[Ltrmodel]] = db.run(ltrmodels.result)
  
  def count(): Int = ltrmodels.length.asInstanceOf[Int]

  def get(mid: Int): Future[Ltrmodel] = {
    val query = ltrmodels.filter(_.mid === mid)
    db.run(query.result.head)
  }

  def get(ltrid: Int, runid: Int): Future[Ltrmodel] = {
    db.run(ltrmodels.filter(_.ltrid === ltrid).filter(_.runid === runid).result.head)
  }

  def insert(Ltrmodel: Ltrmodel): Future[Ltrmodel] = {
    val LtrmodelWithId = (ltrmodels returning ltrmodels.map(_.mid) into ((Ltrmodel, id) => Ltrmodel.copy(mid=Some(id)))) += Ltrmodel
    db.run(LtrmodelWithId)
  }

  def update(Ltrmodel: Ltrmodel): Future[Int] = {
    val query = ltrmodels.filter(_.mid === Ltrmodel.mid)
    db.run(query.update(Ltrmodel))
  }

  def delete(mid: Int): Future[Int] = {
    val query = ltrmodels.filter(_.mid === mid)
    val res = db.run(query.delete)
    res
  }
  
  def fetch(sort: String, order: String, offset: Int = 0, size: Int = 10): Future[Seq[Ltrmodel]] = {
    sort match {
      case "mid" =>
        order match {
          case "asc" =>
            db.run(ltrmodels.sortBy(_.mid.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(ltrmodels.sortBy(_.mid.desc).drop(offset).take(size).result)
        }
    }
  }

  def fetchByLtrid(ltrid: Int): Future[Seq[Ltrmodel]] = {
    db.run(ltrmodels.filter(_.ltrid === ltrid).sortBy(_.runid.desc).result)
  }

  def nextRunId(ltrid: Int): Int = {
    val f = db.run(sql"select max(runid)+1 as n from ltrmodels".as[Int].head)
    val res = Await.result(f, scala.concurrent.duration.Duration.Inf)
    if (res > 0) res else 1
  }

  def deleteByLtrid(ltrid: Int): Future[Int] = {
    val query = ltrmodels.filter(_.ltrid === ltrid)
    val res = db.run(query.delete)
    res
  }
}
