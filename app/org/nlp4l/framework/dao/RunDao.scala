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
import org.joda.time.DateTime
import org.nlp4l.framework.models.Cell
import org.nlp4l.framework.models.CellAttribute
import org.nlp4l.framework.models.CellType
import org.nlp4l.framework.builtin.DbModels.resultAsStringMap
import org.nlp4l.framework.models.Dictionary
import org.nlp4l.framework.models.DictionaryAttribute
import org.nlp4l.framework.models.Record
import org.nlp4l.framework.builtin.Job
import org.nlp4l.framework.builtin.JobStatus

class RunDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends HasDatabaseConfigProvider[JdbcProfile] { 
  import driver.api._
  
  private val logger = Logger(this.getClass)
  
  // Job Status Table
  class JobStatusTable(tag: Tag) extends Table[JobStatus](tag, "JOBSTATUS") {
    def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
    def jobId = column[Int]("JOBID")
    def runId = column[Int]("RUNID")
    def total = column[Int]("TOTAL")
    def done = column[Int]("DONE")
    def message = column[String]("MESSAGE")
    def * = (id.?, jobId, runId, total, done, message) <> (JobStatus.tupled, JobStatus.unapply)
  }

  val jobStatus = TableQuery[JobStatusTable]
  
  def initJobStatus = db.run(jobStatus.schema.create)
  
  def fetchAllJobStatus(): Future[Seq[JobStatus]] = db.run(jobStatus.result)

  def getJobStatusById(id: Int): Future[JobStatus] = {
    val query = jobStatus.filter(_.id === id)
    db.run(query.result.head)
  }

  def insertJobStatus(js: JobStatus): Future[JobStatus] = {
    val jsWithId = (jobStatus returning jobStatus.map(_.id) into ((js, tmpid) => js.copy(id=Some(tmpid)))) += js
    db.run(jsWithId)
  }

  def updateJobStatus(js: JobStatus): Future[Int] = {
    val query = jobStatus.filter(_.id === js.id)
    db.run(query.update(js))
  }

  def deleteJobStatusById(id: Int): Future[Int] = {
    val query = jobStatus.filter(_.id === id)
    db.run(query.delete)
  }
  
  def deleteJobStatusByJobId(jobId: Int): Future[Int] = {
    val query = jobStatus.filter(_.jobId === jobId)
    db.run(query.delete)
  }
  
  def deleteJobStatusByJobIdAndRunId(jobId: Int, runId: Int): Future[Int] = {
    val query = jobStatus.filter(x => x.jobId === jobId && x.runId === runId)
    db.run(query.delete)
  }
  
  def fetchJobStatus(sort: String, order: String, offset: Int = 0, size: Int = 10): Future[Seq[JobStatus]] = {
    sort match {
      case "id" =>
        order match {
          case "asc" =>
            db.run(jobStatus.sortBy(_.id.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobStatus.sortBy(_.id.desc).drop(offset).take(size).result)
        }
      case "jobId" =>
        order match {
          case "asc" =>
            db.run(jobStatus.sortBy(_.jobId.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobStatus.sortBy(_.jobId.desc).drop(offset).take(size).result)
        }
      case "runId" =>
        order match {
          case "asc" =>
            db.run(jobStatus.sortBy(_.runId.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobStatus.sortBy(_.runId.desc).drop(offset).take(size).result)
        }
      case "total" =>
        order match {
          case "asc" =>
            db.run(jobStatus.sortBy(_.total.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobStatus.sortBy(_.total.desc).drop(offset).take(size).result)
        }
      case "done" =>
        order match {
          case "asc" =>
            db.run(jobStatus.sortBy(_.done.asc).drop(offset).take(size).result)
          case "desc" =>
            db.run(jobStatus.sortBy(_.done.desc).drop(offset).take(size).result)
        }
    }
  }
  
  
  // Run Result Table
  
  def createTable(jobId: Int, runId: Int, dicAttr: DictionaryAttribute): Future[Int] = {
    val tableName = s"run_${jobId}_${runId}"
    val b = new StringBuilder
    
    b.append(s"create table ${tableName} (")
    dicAttr.cellAttributeList foreach { c:CellAttribute =>
      val columnName = c.name.toLowerCase()
      c.cellType match {
        case CellType.IntType => b.append(s"  ${columnName} int,")
        case CellType.DateType => b.append(s"  ${columnName} datetime,")
        case CellType.DoubleType => b.append(s"  ${columnName} double precision,")
        case CellType.FloatType => b.append(s"  ${columnName} float,")
        case CellType.StringType => b.append(s"  ${columnName} text,")
      }
    }
    b.append(s"  id int not null primary key,")
    b.append(s"  replay varchar(10),")
    b.append(s"  hashcode int")
    b.append(s")")
    val sql: String = b.toString()

    logger.debug(sql)
    db.run(sqlu"#$sql")

  }
  
  
  def dropTable(jobId: Int, runId: Int): Future[Int] = {
    val sql: String = s"drop table run_${jobId}_${runId}"
    logger.debug(sql)
    db.run(sqlu"#$sql")

  }
  
  def insertData(jobId: Int, runId: Int, dicAttr: DictionaryAttribute, dic: Dictionary): Int  = {
    val tableName = s"run_${jobId}_${runId}"
    var n: Int = 0
    dic.recordList foreach { r: Record =>
      n = n+1
      val hashcode = r.hashCode
      val b = new StringBuffer
      b.append(s"insert into ${tableName} (")
      dicAttr.cellAttributeList foreach { c:CellAttribute =>
        val columnName = c.name.toLowerCase()
        b.append(s"${columnName},")
      }
      b.append(s"id,replay,hashcode) values (")
      r.cellList foreach { c: Cell =>
        Option(c.value) match {
          case Some(cc) => {
            b.append(quote(cc.value.toString()))
            b.append(",")
//            b.append("'")
//            b.append(cc.value.toString().replace("'", "\'"))
//            b.append("',")
          }
          case None => {
            b.append("null,")
          }
        }
      }
      b.append(s"$n,")
      // replay
      var replay = ""
      if(dicAttr.addedRecordList.contains(hashcode)) {
        replay = "ADD"
      }
      if(dicAttr.modifiedRecordList.contains(hashcode)) {
        replay = "MOD"
      }
      if(dicAttr.modifiedRecordList.values.exists(_ == r)) {
        replay = "MOD"
      }
      if(dicAttr.deletedRecordList.contains(hashcode)) {
        replay = "DEL"
      }
      b.append(s"'${replay}',")
      b.append(s"${hashcode})")
      val sql = b.toString()
      
      logger.debug(sql)
      Await.result(db.run(sqlu"#$sql"), scala.concurrent.duration.Duration.Inf)

    }
    n
  }
  
  
  
  def addRecord(jobId: Int, runId: Int, dicAttr: DictionaryAttribute, r: Record): Future[Int]  = {
    val tableName = s"run_${jobId}_${runId}"

    val f: Future[Int] = nextId(jobId, runId)
    val n: Int = Await.result(f, scala.concurrent.duration.Duration.Inf)
    val hashcode = r.hashCode
    val b = new StringBuffer
    b.append(s"insert into ${tableName} (")
    dicAttr.cellAttributeList foreach { c:CellAttribute =>
      val columnName = c.name.toLowerCase()
      b.append(s"${columnName},")
    }
    b.append(s"id,replay,hashcode) values (")
    r.cellList foreach { c: Cell =>
      Option(c.value) match {
        case Some(cc) => {
            b.append(quote(cc.value.toString()))
            b.append(",")
//          b.append("'")
//          b.append(cc.value.toString().replace("'", "\'"))
//          b.append("',")
        }
        case None => {
          b.append("null,")
        }
      }
    }

    // id
    b.append(s"'${n}',")
    // replay
    val replay = "ADD"
    b.append(s"'${replay}',")
    b.append(s"${hashcode})")
    val sql = b.toString()
    
    logger.debug(sql)
    db.run(sqlu"#$sql")
      
    Future(n)
  }
  
  
  def updateRecord(jobId: Int, runId: Int, recordId: Int, dicAttr: DictionaryAttribute, r: Record): Future[Int]  = {
    val tableName = s"run_${jobId}_${runId}"
    val hashcode = r.hashCode
    val b = new StringBuffer
    b.append(s"update ${tableName} set")
    r.cellList foreach { c: Cell =>
      val columnName = c.name.toLowerCase()
      b.append(s" ${columnName} = ")
      Option(c.value) match {
        case Some(cc) => {
          cc.value match {
            case cv: String  => {
              b.append("'")
              b.append(cc.value.toString().replace("'", "\'"))
              b.append("',")
            }
            case cv: DateTime  => {
              b.append("'")
              b.append(cc.value.toString())
              b.append("',")
            }
            case _ => {
              b.append(cc.value.toString())
              b.append(",")
            }
          }
          
        }
        case None => {
          b.append("null,")
        }
      }
    }

    // replay
    val replay = "MOD"
    b.append(s" replay='${replay}',")
    b.append(s" hashcode=${hashcode}")
    b.append(s" where id=${recordId}")
    val sql = b.toString()
    
    logger.debug(sql)
    db.run(sqlu"#$sql")

    Future(recordId)
  }
  
  def selectRunList(jobId: Int, lastRunId: Int): Seq[Int]  = {
    val res = ListBuffer.empty[Int]
    (1 to lastRunId) foreach { runId =>
      val tableName = s"run_${jobId}_${runId}"
      var t = Await.result(db.run(MTable.getTables(tableName.toUpperCase()).headOption ), scala.concurrent.duration.Duration.Inf)
      if(t == None) {
        t = Await.result(db.run(MTable.getTables(tableName.toLowerCase()).headOption ), scala.concurrent.duration.Duration.Inf)
      }
      t map { x =>
        runId +=: res
      }
      
    }
    res
  }
  

  def totalCount(jobId: Int, runId: Int): Future[Int] = {
    db.run(sql"select count(id) as n from run_#${jobId}_#${runId}".as[Int].head)
  }
  
  def nextId(jobId: Int, runId: Int): Future[Int] = {
    db.run(sql"select max(id)+1 as n from run_#${jobId}_#${runId}".as[Int].head)
  }

  def totalCountFilter(jobId: Int, runId: Int, filters: Map[String, String]): Future[Int] = {
    val cond0: String = filters.map { case(k,v) => "%s = %s" format (k, quote(v))} . mkString(" and ")
    val cond: String = filters.map { 
      case(k,v) if v.startsWith("*") && v.endsWith("*") => "%s like %s" format (k, quote("%"+v.substring(1, if(v.length() > 1) { v.length()-1 } else {v.length()})+"%"))
      case(k,v) if v.startsWith("*") => "%s like %s" format (k, quote("%"+v.substring(1, v.length())))
      case(k,v) if v.endsWith("*") => "%s like %s" format (k, quote(v.substring(0, if(v.length() > 1) { v.length()-1 } else {v.length()})+"%"))
      case(k,v) => "%s = %s" format (k, quote(v))
    } . mkString(" and ")
    val filter: String = cond match {
      case "" => ""
      case _ => " where "  + cond
    }
    
    db.run(sql"select count(id) as n from run_#${jobId}_#${runId}#${filter}".as[Int].head)
  }
  
  def fetch(tableName: String, job: Job, dic: DictionaryAttribute, sort: String, order: String, offset: Int = 0, size: Int = 10, filters: Map[String, String]): Dictionary = {
    val cond: String = filters.map { 
      case(k,v) if v.startsWith("*") && v.endsWith("*") => "%s like %s" format (k, quote("%"+v.substring(1, if(v.length() > 1) { v.length()-1 } else {v.length()})+"%"))
      case(k,v) if v.startsWith("*") => "%s like %s" format (k, quote("%"+v.substring(1, v.length())))
      case(k,v) if v.endsWith("*") => "%s like %s" format (k, quote(v.substring(0, if(v.length() > 1) { v.length()-1 } else {v.length()})+"%"))
      case(k,v) => "%s = %s" format (k, quote(v))
    } . mkString(" and ")
    val filter: String = cond match {
      case "" => ""
      case _ => " where "  + cond
    }
        
    var colTypeMap: Map[String, String] = Map()
    val colOrder = ListBuffer.empty[String]
    val selectSql = new StringBuilder("select")
    var t = Await.result(db.run(MTable.getTables(tableName.toUpperCase()).headOption ), scala.concurrent.duration.Duration.Inf)
    if(t == None) {
      t = Await.result(db.run(MTable.getTables(tableName.toLowerCase()).headOption ), scala.concurrent.duration.Duration.Inf)
    }
    t map { tt =>
      val cols = Await.result(db.run( tt.getColumns ), scala.concurrent.duration.Duration.Inf)
      for (col: MColumn <- cols) {
        selectSql append s" ${col.name.toLowerCase()},"
        colTypeMap += (col.name.toLowerCase() -> col.sqlTypeName.getOrElse(""))
        colOrder += col.name.toLowerCase()
      }
      selectSql append s" id from ${tableName}${filter}"
      selectSql append s" order by ${sort} ${order} limit ${size} offset ${offset}"
    }
    val r = Await.result(db.run(sql"#$selectSql".as[Map[String, Any]]), scala.concurrent.duration.Duration.Inf)
    val records = for(rr:Map[String, Any] <- r) yield {
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
      Record(cells)
    }

    Dictionary(records)
  }
  
  
  def fetchAll(jobId: Int, runId: Int, sort: String = "id", order: String = "asc"): Dictionary = {
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
      selectSql += s" from ${tableName}"
      selectSql += s" order by ${sort} ${order}"
    }
    
    logger.debug(selectSql)
    val r = Await.result(db.run(sql"#$selectSql".as[Map[String, Any]]), scala.concurrent.duration.Duration.Inf)
    val records = for(rr:Map[String, Any] <- r) yield {
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
      Record(cells)
    }

    Dictionary(records)
  }
  
  def fetchRecordById(jobId: Int, runId: Int, recordId: Int): Option[Record] = {
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
          selectSql += s" ${colname},"
          colTypeMap += (col.name.toLowerCase() -> col.sqlTypeName.getOrElse(""))
          colOrder += col.name.toLowerCase()
        }
      }
      selectSql = selectSql.stripSuffix(",")
      selectSql += s" from ${tableName} where id=${recordId}"
    }

    logger.debug(selectSql)
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
  
  
  
  def fetchRecordData(jobId: Int, runId: Int, recordId: Int): Map[String, Any] = {
    val tableName = s"run_${jobId}_${runId}"
    val selectSql = new StringBuilder("select")
    var t = Await.result(db.run(MTable.getTables(tableName.toUpperCase()).headOption ), scala.concurrent.duration.Duration.Inf)
    if(t == None) {
      t = Await.result(db.run(MTable.getTables(tableName.toLowerCase()).headOption ), scala.concurrent.duration.Duration.Inf)
    }
    t map { tt =>
      val cols = Await.result(db.run( tt.getColumns ), scala.concurrent.duration.Duration.Inf)
      for (col: MColumn <- cols) {
        selectSql append s" ${col.name.toLowerCase()},"
      }
      selectSql append s" id from ${tableName} where id=${recordId}"
    }

    logger.debug(selectSql.toString)
    Await.result(db.run(sql"#$selectSql".as[Map[String, Any]].head), scala.concurrent.duration.Duration.Inf)
  }
  
  def fetchCellValueList(jobId: Int, runId: Int, cellname: String): List[Any] = {
    val tableName = s"run_${jobId}_${runId}"
    val selectSql = s"select distinct ${cellname} from ${tableName} order by ${cellname} asc"
    
    logger.debug(selectSql)
    val filterlist: List[Any] = List()
    val r = Await.result(db.run(sql"#$selectSql".as[Map[String,Any]]), scala.concurrent.duration.Duration.Inf)
    val res = ListBuffer.empty[Any]
    r.foreach { rr: Map[String, Any] =>
      res += rr.values.head
    }
    res.toList
  }
  
  def fetchRecordHashcode(jobId: Int, runId: Int, recordId: Int): Int = {
    val tableName = s"run_${jobId}_${runId}"
    val selectSql = new StringBuilder("select")
    var t = Await.result(db.run(MTable.getTables(tableName.toUpperCase()).headOption ), scala.concurrent.duration.Duration.Inf)
    if(t == None) {
      t = Await.result(db.run(MTable.getTables(tableName.toLowerCase()).headOption ), scala.concurrent.duration.Duration.Inf)
    }
    t map { tt =>
      val cols = Await.result(db.run( tt.getColumns ), scala.concurrent.duration.Duration.Inf)
      for (col: MColumn <- cols) {
        selectSql append s" ${col.name.toLowerCase()},"
      }
      selectSql append s" id from ${tableName} where id=${recordId}"
    }
    Await.result(db.run(sql"select hashcode from run_#${jobId}_#${runId} where id=#${recordId}".as[Int].head), scala.concurrent.duration.Duration.Inf)
  }
  
  
  def deleteRecord(jobId: Int, runId: Int, recordId: Int): Future[Int] = {
    db.run(sqlu"delete from run_#${jobId}_#${runId} where id=#${recordId}")
  }  
  
  def fetchAllColumn(jobId: Int, runId: Int): Dictionary = {
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
        if(colname != "hashcode") {
          selectSql += s" ${col.name.toLowerCase()},"
          colTypeMap += (col.name.toLowerCase() -> col.sqlTypeName.getOrElse(""))
          colOrder += col.name.toLowerCase()
        }
      }
      selectSql = selectSql.stripSuffix(",")
      selectSql += s" from ${tableName}"
    }

    val r = Await.result(db.run(sql"#$selectSql".as[Map[String, Any]]), scala.concurrent.duration.Duration.Inf)

    val records = for(rr:Map[String, Any] <- r) yield {
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
      Record(cells)
    }

    Dictionary(records)
  }
  
  def quote(v: String): String = {
    val s = new StringBuilder(v.length + 4) append '''
    for(c <- v) if(c == ''') s append "\'\'" else s append c
    s append ''' toString
  }

}
