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
import org.nlp4l.ltr.support.models.FeatureProgress



class FeatureProgressDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] {
  import driver.api._

  private val logger = Logger(this.getClass)
  
  class FeatureProgressTable(tag: Tag) extends Table[FeatureProgress](tag, "FEATUREPROGRESS") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
    def ltrid = column[Int]("LTRID")
    def progress = column[Int]("PROGRESS")
    def message = column[String]("MESSAGE")
    def * = (id.?, ltrid, progress, message) <> (FeatureProgress.tupled, FeatureProgress.unapply)
  }

  val featureprogress = TableQuery[FeatureProgressTable]

  def init = db.run(featureprogress.schema.create)

  def fetchAll(): Future[Seq[FeatureProgress]] = db.run(featureprogress.result)
  
  def count(): Int = featureprogress.length.asInstanceOf[Int]

  def get(id: Int): Future[FeatureProgress] = {
    val query = featureprogress.filter(_.id === id)
    db.run(query.result.head)
  }

  def insert(featureStatus: FeatureProgress): Future[FeatureProgress] = {
    val featureStatusWithId = (featureprogress returning featureprogress.map(_.id) into ((FeatureStatus, id) => FeatureStatus.copy(id=Some(id)))) += featureStatus
    db.run(featureStatusWithId)
  }
  
  def getByLtrid(ltrid: Int): Future[FeatureProgress] = {
    val query = featureprogress.filter(_.ltrid === ltrid)
    db.run(query.result.head)
  }

  def update(featureStatus: FeatureProgress): Future[Int] = {
    val query = featureprogress.filter(_.id === featureStatus.id)
    db.run(query.update(featureStatus))
  }

  def delete(id: Int): Future[Int] = {
    val query = featureprogress.filter(_.id === id)
    val res = db.run(query.delete)
    res
  }
  
  def deleteByLtrid(ltrid: Int): Future[Int] = {
    val query = featureprogress.filter(_.ltrid === ltrid)
    val res = db.run(query.delete)
    res
  }
  
  def fetch(sort: String, order: String, offset: Int = 0, size: Int = 10): Future[Seq[FeatureProgress]] = {
    sort match {
      case "id" =>
        order match {
          case "asc" =>
            db.run(featureprogress.sortBy(_.id.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(featureprogress.sortBy(_.id.desc).drop(offset).take(size).result)
        }
    }
  }

}
