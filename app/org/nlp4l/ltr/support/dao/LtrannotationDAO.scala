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

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

import org.nlp4l.ltr.support.models.Ltrannotation

import javax.inject.Inject
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.lifted.ProvenShape.proveShapeOf



class LtrannotationDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] {
  import driver.api._

  private val logger = Logger(this.getClass)
  
  class LtrannotationTable(tag: Tag) extends Table[Ltrannotation](tag, "LTRANNOTATIONS") {
    def qid = column[Int]("QID")
    def docid = column[String]("DOCID")
    def label = column[Int]("LABEL")
    def ltrid = column[Int]("LTRID")
    def * = (qid, docid, label, ltrid) <> (Ltrannotation.tupled, Ltrannotation.unapply)
    def pk = primaryKey("PK_LTRANNOTATIONS", (qid, docid))
  }

  val ltrannotations = TableQuery[LtrannotationTable]

  def init = db.run(ltrannotations.schema.create)

  def fetchAll(): Future[Seq[Ltrannotation]] = db.run(ltrannotations.result)
  
  def count(): Int = ltrannotations.length.asInstanceOf[Int]

  def get(qid: Int, docid: String): Future[Ltrannotation] = {
    val query = ltrannotations.filter(_.qid === qid).filter(_.docid === docid)
    db.run(query.result.head)
  }

  def getByQid(qid: Int): Future[Seq[Ltrannotation]] = {
    val query = ltrannotations.filter(_.qid === qid)
    db.run(query.result)
  }

  def insert(ltrannotation: Ltrannotation): Future[Ltrannotation] = {
    val LtrannotationWithId = (ltrannotations returning ltrannotations.map(_.qid) into ((ltrannotation, id) => ltrannotation.copy(qid=id))) += ltrannotation
    db.run(LtrannotationWithId)
  }

  def insertList(ltrannotationList: Seq[Ltrannotation]): Unit = {
    val query = ltrannotations ++= ltrannotationList
    Await.result(db.run(query), scala.concurrent.duration.Duration.Inf)
  }

  def deleteByQid(qid: Int): Future[Int] = {
    val query = ltrannotations.filter(_.qid === qid)
    db.run(query.delete)
  }

  def deleteByLtrid(ltrid: Int): Future[Int] = {
    val query = ltrannotations.filter(_.ltrid === ltrid)
    db.run(query.delete)
  }

  def update(Ltrannotation: Ltrannotation): Future[Int] = {
    val query = ltrannotations.filter(_.qid === Ltrannotation.qid).filter(_.docid === Ltrannotation.docid)
    db.run(query.update(Ltrannotation))
  }

  def delete(qid: Int, docid: String): Future[Int] = {
    val query = ltrannotations.filter(_.qid === qid).filter(_.docid === docid)
    val res = db.run(query.delete)
    res
  }
  
  def fetch(sort: String, order: String, offset: Int = 0, size: Int = 10): Future[Seq[Ltrannotation]] = {
    sort match {
      case "qid" =>
        order match {
          case "asc" =>
            db.run(ltrannotations.sortBy(_.qid.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(ltrannotations.sortBy(_.qid.desc).drop(offset).take(size).result)
        }
    }
  }

  def fetchByLtrid(ltrid: Int): Future[Seq[Ltrannotation]] = {
    db.run(ltrannotations.filter(_.ltrid === ltrid).result)
  }
}
