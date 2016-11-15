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
import org.nlp4l.ltr.support.models.Ltrquery
import javax.inject.Inject
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.lifted.ProvenShape.proveShapeOf
import javax.inject.Inject



class LtrqueryDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] {
  import driver.api._

  private val logger = Logger(this.getClass)
  
  class LtrqueryTable(tag: Tag) extends Table[Ltrquery](tag, "LTRQUERIES") {
    def qid = column[Int]("QID", O.PrimaryKey, O.AutoInc)
    def query = column[String]("QUERY")
    def ltrid = column[Int]("LTRID")
    def checked_flg = column[Boolean]("CHECKED_FLG")
    def * = (qid.?, query, ltrid, checked_flg) <> (Ltrquery.tupled, Ltrquery.unapply)
  }

  val ltrqueries = TableQuery[LtrqueryTable]

  def init = db.run(ltrqueries.schema.create)

  def fetchAll(): Future[Seq[Ltrquery]] = db.run(ltrqueries.result)
  
  def count(): Int = ltrqueries.length.asInstanceOf[Int]

  def get(qid: Int): Future[Ltrquery] = {
    val query = ltrqueries.filter(_.qid === qid)
    db.run(query.result.head)
  }

  def insert(Ltrquery: Ltrquery): Future[Ltrquery] = {
    val LtrqueryWithId = (ltrqueries returning ltrqueries.map(_.qid) into ((Ltrquery, id) => Ltrquery.copy(qid=Some(id)))) += Ltrquery
    db.run(LtrqueryWithId)
  }

  def update(Ltrquery: Ltrquery): Future[Int] = {
    val query = ltrqueries.filter(_.qid === Ltrquery.qid)
    db.run(query.update(Ltrquery))
  }

  def delete(qid: Int): Future[Int] = {
    val query = ltrqueries.filter(_.qid === qid)
    val res = db.run(query.delete)
    res
  }
  
  def fetch(sort: String, order: String, offset: Int = 0, size: Int = 10): Future[Seq[Ltrquery]] = {
    sort match {
      case "qid" =>
        order match {
          case "asc" =>
            db.run(ltrqueries.sortBy(_.qid.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(ltrqueries.sortBy(_.qid.desc).drop(offset).take(size).result)
        }
    }
  }

  def fetchByLtrid(ltrid: Int, sort: String = "qid", order: String= "asc", offset: Int = 0, size: Int = 10): Future[Seq[Ltrquery]] = {
    sort match {
      case "query" =>
        order match {
          case "asc" =>
            db.run(ltrqueries.filter(_.ltrid === ltrid).sortBy(_.query.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(ltrqueries.filter(_.ltrid === ltrid).sortBy(_.query.desc).drop(offset).take(size).result)
        }
      case "checked_flg" =>
        order match {
          case "asc" =>
            db.run(ltrqueries.filter(_.ltrid === ltrid).sortBy(_.checked_flg.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(ltrqueries.filter(_.ltrid === ltrid).sortBy(_.checked_flg.desc).drop(offset).take(size).result)
        }
      case _ => {
        order match {
          case "asc" =>
            db.run(ltrqueries.filter(_.ltrid === ltrid).sortBy(_.qid.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(ltrqueries.filter(_.ltrid === ltrid).sortBy(_.qid.desc).drop(offset).take(size).result)
        }
      }
    }
  }

  def totalCountByLtrid(ltrid: Int): Future[Int] = {
    db.run(ltrqueries.filter(_.ltrid === ltrid).length.result)
  }

  def totalSavedCountByLtrid(ltrid: Int): Future[Int] = {
    db.run(ltrqueries.filter(_.ltrid === ltrid).filter(_.checked_flg === true).length.result)
  }

  def fetchNext(ltrid: Int, qid: Int): Future[Option[Ltrquery]] = {
    val query = ltrqueries.filter(_.ltrid === ltrid).filter(_.qid > qid).filter(_.checked_flg === false).sortBy(_.qid.asc)
    db.run(query.result.headOption)
  }

  def clearCheckedFlg(ltrid: Int): Future[Int] = {
    val sql: String = s"update ltrqueries set checked_flg = false where ltrid = ${ltrid}"
    db.run(sqlu"#$sql")
  }

  def deleteByLtrid(ltrid: Int): Future[Int] = {
    val query = ltrqueries.filter(_.ltrid === ltrid)
    db.run(query.delete)
  }


}
