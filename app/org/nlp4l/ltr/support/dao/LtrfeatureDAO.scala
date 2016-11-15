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


import javax.inject.Inject
import play.api.Logger
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.lifted.ProvenShape.proveShapeOf
import org.nlp4l.ltr.support.models.Ltrfeature



class LtrfeatureDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] {
  import driver.api._

  private val logger = Logger(this.getClass)
  
  class LtrfeatureTable(tag: Tag) extends Table[Ltrfeature](tag, "LTRFEATURE") {
    def fid = column[Int]("FID", O.PrimaryKey, O.AutoInc)
    def ltrid = column[Int]("LTRID")
    def name = column[String]("NAME")
    def * = (fid.?, ltrid, name) <> (Ltrfeature.tupled, Ltrfeature.unapply)
  }

  val ltrfeatures = TableQuery[LtrfeatureTable]

  def init = db.run(ltrfeatures.schema.create)

  def fetchAll(): Future[Seq[Ltrfeature]] = db.run(ltrfeatures.result)
  
  def count(): Int = ltrfeatures.length.asInstanceOf[Int]

  def get(fid: Int): Future[Ltrfeature] = {
    val query = ltrfeatures.filter(_.fid === fid)
    db.run(query.result.head)
  }

  def insert(ltrfeature: Ltrfeature): Future[Ltrfeature] = {
    val featureWithId = (ltrfeatures returning ltrfeatures.map(_.fid) into ((Feature, id) => Feature.copy(fid=Some(id)))) += ltrfeature
    db.run(featureWithId)
  }

  def update(ltrfeature: Ltrfeature): Future[Int] = {
    val query = ltrfeatures.filter(_.fid === ltrfeature.fid)
    db.run(query.update(ltrfeature))
  }

  def delete(fid: Int): Future[Int] = {
    val query = ltrfeatures.filter(_.fid === fid)
    val res = db.run(query.delete)
    res
  }
  
  def deleteByLtrid(ltrid: Int): Future[Int] = {
    val query = ltrfeatures.filter(_.ltrid === ltrid)
    val res = db.run(query.delete)
    res
  }
  
  def fetch(sort: String, order: String, offset: Int = 0, size: Int = 10): Future[Seq[Ltrfeature]] = {
    sort match {
      case "fid" =>
        order match {
          case "asc" =>
            db.run(ltrfeatures.sortBy(_.fid.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(ltrfeatures.sortBy(_.fid.desc).drop(offset).take(size).result)
        }
    }
  }

  def fetchByLtrid(ltrid: Int): Future[Seq[Ltrfeature]] = {
    db.run(ltrfeatures.filter(_.ltrid === ltrid).sortBy(_.fid.asc).result)
  }
  

}
