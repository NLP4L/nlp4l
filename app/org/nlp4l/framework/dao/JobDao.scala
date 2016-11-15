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
package org.nlp4l.framework.dao

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
import org.nlp4l.framework.models.Cell
import org.nlp4l.framework.builtin.DbModels.resultAsStringMap
import org.nlp4l.framework.models.Record
import org.nlp4l.framework.builtin.Replay
import org.nlp4l.framework.builtin.Job


class JobDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] {
  import driver.api._

  private val logger = Logger(this.getClass)
  
  class JobTable(tag: Tag) extends Table[Job](tag, "JOBS") {
    def jobId = column[Int]("JOBID", O.PrimaryKey, O.AutoInc)
    def name = column[String]("NAME")
    def config = column[String]("CONFIG")
    def lastRunId = column[Int]("LASTRUNID")
    def lastRunAt = column[Option[DateTime]]("LASTRUNAT")
    def lastDeployAt = column[Option[DateTime]]("LASTDEPLOYAT")
    def * = (jobId.?, name, config, lastRunId, lastRunAt, lastDeployAt) <> (Job.tupled, Job.unapply)
  }

  val jobs = TableQuery[JobTable]

  def init = db.run(jobs.schema.create)

  def fetchAll(): Future[Seq[Job]] = db.run(jobs.result)
  
  def count(): Int = jobs.length.asInstanceOf[Int]

  def get(jobId: Int): Future[Job] = {
    val query = jobs.filter(_.jobId === jobId)
    db.run(query.result.head)
  }

  def insert(job: Job): Future[Job] = {
    val jobWithId = (jobs returning jobs.map(_.jobId) into ((job, id) => job.copy(jobId=Some(id)))) += job
    val res = db.run(jobWithId)
    
    res map { newJob =>
      newJob.jobId map { j =>
        createReplayTable(j)
      }
    }
    res
  }

  def update(job: Job): Future[Int] = {
    val query = jobs.filter(_.jobId === job.jobId)
    db.run(query.update(job))
  }

  def delete(jobId: Int): Future[Int] = {
    val query = jobs.filter(_.jobId === jobId)
    val res = db.run(query.delete)
    res
  }
  
  def fetch(sort: String, order: String, offset: Int = 0, size: Int = 10): Future[Seq[Job]] = {
    sort match {
      case "jobId" =>
        order match {
          case "asc" =>
            db.run(jobs.sortBy(_.jobId.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobs.sortBy(_.jobId.desc).drop(offset).take(size).result)
        }
      case "name" =>
        order match {
          case "asc" =>
            db.run(jobs.sortBy(_.name.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobs.sortBy(_.name.desc).drop(offset).take(size).result)
        }
      case "lastRunAt" =>
        order match {
          case "asc" =>
            db.run(jobs.sortBy(_.lastRunAt.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobs.sortBy(_.lastRunAt.desc).drop(offset).take(size).result)
        }
      case "lastDeployAt" =>
        order match {
          case "asc" =>
            db.run(jobs.sortBy(_.lastDeployAt.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobs.sortBy(_.lastDeployAt.desc).drop(offset).take(size).result)
        }
    }
  }

  
  
  
  //
  // Tables for replay
  //
  def createReplayTable(jobId: Int): Future[Int] = {
    val tableName = s"replay_${jobId}"
    
    // hashcode = Original record hashcode
    // replay = ADD, DEL, MOD
    // modToHashcode = Modified record hashcode in case of MOD
    val sql = s"""
      create table ${tableName} (
        runId int,
        hashcode int,
        replay varchar(10),
        modToHashcode int,
        CONSTRAINT u_key_${tableName} UNIQUE (runId, hashcode)
      )
    """
        
    logger.debug(sql)
    db.run(sqlu"#$sql")

  }
  
  def dropReplayTable(jobId: Int): Future[Int] = {
    val tableName = s"replay_${jobId}"
    
    val sql = s"drop table ${tableName}"
   
    logger.debug(sql)
    db.run(sqlu"#$sql")

  }
  
  def deleteReplayTable(jobId: Int, runId: Int): Future[Int] = {
    val tableName = s"replay_${jobId}"
    
    val sql = s"delete from ${tableName} where runId = ${runId}"
    
    logger.debug(sql)
    db.run(sqlu"#$sql")

  }
  
  def fetchReplayOfAdd(jobId: Int): List[(Int, Int)] = {
    val tableName = s"replay_${jobId}"
    val selectSql = s"select runId, hashcode from ${tableName} where replay='ADD'"
    val r = Await.result(db.run(sql"#$selectSql".as[(Int, Int)]), scala.concurrent.duration.Duration.Inf)
    r.toList
  }
  
  def fetchReplayOfDel(jobId: Int): List[Int] = {
    val tableName = s"replay_${jobId}"
    val selectSql = s"select hashcode from ${tableName} where replay='DEL'"
    val r = Await.result(db.run(sql"#$selectSql".as[Int]), scala.concurrent.duration.Duration.Inf)
    r.toList
  }
  
  def fetchReplayOfMod(jobId: Int): List[(Int, Int, Int)] = {
    val tableName = s"replay_${jobId}"
    val selectSql = s"select runId, hashcode, modToHashcode from ${tableName} where replay='MOD'"
    val r = Await.result(db.run(sql"#$selectSql".as[(Int, Int, Int)]), scala.concurrent.duration.Duration.Inf)
    r.toList
  }
  
  def insertReplay(jobId: Int, r: Replay): Future[Int] = {
    db.run(sqlu"insert into replay_#${jobId} (runId, hashcode, replay, modToHashcode) values (${r.runId}, ${r.hashcode}, ${r.replay}, ${r.modToHashcode})")
  }
  
  def deleteOldReplay(jobId: Int, runId: Int, hashcode: Int): Future[Int] = {
    db.run(sqlu"delete from replay_#${jobId} where runId=#${runId} and hashcode=#${hashcode}")
  }
  
  def fetchRecordByHashcode(jobId: Int, runId: Int, hashcode: Int): Option[Record] = {
    val tableName = s"run_${jobId}_${runId}"
    var colTypeMap: Map[String, String] = Map()
    val colOrder = ListBuffer.empty[String]
    var selectSql = "select"
    var t = Await.result(db.run(MTable.getTables(tableName.toUpperCase()).headOption ), scala.concurrent.duration.Duration.Inf)
    if(t == None) {
      t = Await.result(db.run(MTable.getTables(tableName.toLowerCase()).headOption ), scala.concurrent.duration.Duration.Inf)
    }
    t map { tt =>
      val cols = Await.result(db.run( tt.getColumns ), scala.concurrent.duration.Duration.Inf)
      
      for (col: MColumn <- cols) {
        val colname = col.name.toLowerCase()
        if(colname != "replay" && colname != "id" && colname != "hashcode") {
          selectSql += s" ${col.name.toLowerCase()},"
          colTypeMap += (col.name.toLowerCase() -> col.sqlTypeName.getOrElse(""))
          colOrder += col.name.toLowerCase()
        }
      }
      selectSql = selectSql.stripSuffix(",")
      selectSql += s" from ${tableName} where hashcode=${hashcode}"
    }

    val r = Await.result(db.run(sql"#$selectSql".as[Map[String, Any]].headOption), scala.concurrent.duration.Duration.Inf)
    r match {
      case Some(rr) => {
        val cells = ListBuffer.empty[Cell]
        colOrder foreach { colName: String =>
          val v: Any = rr.getOrElse(colName, null)
          if(v != null) {
            colTypeMap.get(colName) match {
                case Some("INTEGER") => cells += Cell(colName, v.asInstanceOf[String].toInt)
                case Some("DOUBLE") => cells += Cell(colName, v.asInstanceOf[String].toDouble)
                case Some("FLOAT") => cells += Cell(colName, v.asInstanceOf[String].toFloat)
                case Some("DATE") => cells += Cell(colName, DateTime.parse(v.asInstanceOf[String]))
                case _ => cells += Cell(colName, v.asInstanceOf[String])
            }
          } else {
            cells += Cell(colName, null)
          }
        }
        Some(Record(cells))
      }
      case None => None
    }
  }

}
