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

package org.nlp4l.framework.models

import java.util.Date

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.collection.mutable.ListBuffer

/**
 * Data unit
 */
case class Cell (
    name: String,
    value: Any,
    userDefinedHashCode:(Any) => Int = null
) {
  override def hashCode: Int = {
    if(userDefinedHashCode != null) {
      userDefinedHashCode(value)
    } else {
      val prime:Int = 31
      var result: Int = 1
      if(value == null) {
        result = prime * result + 0
      } else {
        result = prime * result + value.toString().hashCode
      }
      result
    }
  }
  
  /**
   * Merge two cell with glue string when cell value is string
   */
  def merge(that: Cell, glue: String): Cell = {
    this.value match {
      case x: String => {
        (x, that.value) match {
          case (null, null) => Cell(this.name, null)
          case (null, y) => Cell(this.name, y.toString())
          case (x, null) => Cell(this.name, x)
          case (x, y) => Cell(this.name, this.value.toString() + glue + y.toString())
        }
      }
      case _ => this
    }
  }
}

/**
 * Collection of cell, such like row
 */
case class Record (
    cellList: Seq[Cell]
) {

  val cellMap: Map[String, Cell] = cellList.map(cell => (cell.name -> cell)).toMap

  def cellValue(name: String): Option[Any] = {
    cellMap.get(name) match {
      case Some(cell) => Some(cell.value)
      case _ => None
    }
  }

  /**
   * Merge two record having same cell value of key with glue string 
   */
  def merge(key: String, glue:String, that:Record): Record = {
    val cells = for(thisCell <- cellList) yield {
      if(thisCell.name == key) {
        thisCell
      } else {
        val a = that.cellList.filter(_.name == thisCell.name)
        if(a.isEmpty) thisCell
        else{
          thisCell.merge(a.head, glue)
        }
      }
    }
    Record(cells)
  }
  
  def canMerge(key: String, that:Record): Boolean = {
    val a = this.cellList.filter(_.name == key)
    if(a.isEmpty) false
    else{
      val b = that.cellList.filter(_.name == key)
      if(b.isEmpty) false
      else{
        a.head.value == b.head.value
      }
    }
  }
  
  def setUserDefinedHashCode(dicAttr: DictionaryAttribute): Record = {
    val cells = for(thisCell <- cellList) yield {
      val cellAttrs = dicAttr.cellAttributeList.filter(_.name == thisCell.name)
      if(cellAttrs.length > 0){
        if(cellAttrs.head.userDefinedHashCode != null) {
          Cell(thisCell.name, thisCell.value, cellAttrs.head.userDefinedHashCode)
        } else {
          thisCell
        }
      }
      else{
        thisCell
      }
    }
    Record(cells)
  }
  
  override def hashCode: Int = {
    cellList.foldLeft(0){
      //(a, b) => (a ^ b.hashCode) << 1
      (a, b) => (((a.toString.hashCode << 16) + a) ^ b.hashCode) << 1
    }
  }

  def mkCsvRecord(sep: String, notFilters: String = null, surroundQuote: Boolean = true): String = {
    val nfs = if(notFilters == null) Set.empty[String] else notFilters.split(",").toSet
    cellList.filterNot(c => nfs.contains(c.name)).map(c =>
      if(surroundQuote) s""""${c.value.toString}"""" else c.value.toString).mkString(sep)
  }
}

/**
 * Collection of Record
 * Input and output unit
 */
case class Dictionary (
    recordList: Seq[Record] 
) {
  def setUserDefinedHashCode(dicAttr: DictionaryAttribute): Dictionary = {
    val list = recordList.map(r => r.setUserDefinedHashCode(dicAttr))
    Dictionary(list)
  }

  def cellList[A](cellname: String, f: (Any) => A): Seq[A] = {
    recordList.map { r =>
      r.cellMap.get(cellname) match {
        case Some(cell) => {
          f(cell.value)
        }
        case None => throw new IllegalArgumentException(s"""cell name '$cellname' is not found""")
      }
    }
  }
}

trait CellView {
  /**
   * Format a cell value
   * This method called when cell value displayed by framework
   */
  def format(cell: Any): String = {
    if(cell.isInstanceOf[DateTime]) {
      val dcell: DateTime = cell.asInstanceOf[DateTime]
      DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss").print(dcell)
    } else {
      cell.toString()
    }
  }

  /**
   * Convert inputed data to 
   */
  def toInt(edit: String): Int = {
    edit.toInt
  }
  def toFloat(edit: String): Float = {
    edit.toFloat
  }
  def toDouble(edit: String): Double = {
    edit.toDouble
  }
  def toDate(edit: String): Date = {
    DateTime.parse(edit).toDate()
  }
}

case class CellAttribute(
    val name: String,
    val cellType: CellType, // String, Int, Float, Double, DateTime
    val isFilterable: Boolean = false,
    val isSortable: Boolean = false,
    val userDefinedHashCode:(Any) => Int = null // user defined hashCode
    ) extends CellView {
  
  def columnName: String = {
    name.toLowerCase()
  }
}

case class RecordWithAttrbute(record: Record, attribute: DictionaryAttribute)


/**
 * Data type enum for Cell 
 */
object CellType {
  case object StringType extends CellType(0)
  case object IntType extends CellType(1)
  case object FloatType extends CellType(2)
  case object DoubleType extends CellType(3)
  case object DateType extends CellType(4)
}
sealed abstract class CellType(val code: Int) {
  val name = toString
}


class DictionaryAttribute(val name: String, val cellAttributeList: Seq[CellAttribute]) {

  // These lists are for Replay Processor
  var addedRecordList: Map[Int, Record] = Map()      // New added record hashCode and record data
  var deletedRecordList: List[Int] = List()      // Deleted record hashCode
  var modifiedRecordList: Map[Int, Record] = Map()      // Original record hashCode and modified record data
  
  def getCellAttribute(name: String): Option[CellAttribute] = {
    cellAttributeList.filter(_.name.toLowerCase() == name.toLowerCase()).headOption
  }
}
