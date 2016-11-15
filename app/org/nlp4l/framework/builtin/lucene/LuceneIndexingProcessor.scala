/*
 * Copyright 2016 org.NLP4L
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

package org.nlp4l.framework.builtin.lucene

import com.typesafe.config.Config
import org.nlp4l.framework.models.{Record, _}
import org.nlp4l.framework.processors._
import org.nlp4l.lucene._

import scala.collection.mutable.ArrayBuffer

class LuceneIndexingProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new LuceneIndexingProcessor(
      getConfigParam("schemaDef", null),
      getStrParam("schemaFile", null),
      getStrParamRequired("index"),
      getConfigListParam("fieldsMap", null),
      getBoolParam("deleteAll", true),
      getBoolParam("optimize", true)
    )
  }
}

class LuceneIndexingProcessor(val schemaDef: Config,
                                 val schemaFile: String,
                                 val index: String,
                                 val fieldsMapConf: Seq[Config],
                                 val deleteAll: Boolean,
                                 val optimize: Boolean)
  extends Processor {

  val embedded = new LuceneIndexingProcessorEmbedded(schemaDef, schemaFile, index, fieldsMapConf, deleteAll, optimize)

  override def execute(data: Option[Dictionary]): Option[Dictionary] = {

    embedded.write(data.get.recordList)

    val result = s"success: ${data.get.recordList.size} documents added."

    val dict = Dictionary(Seq(
      Record(Seq(
        Cell("result", result)
    ))))
    Some(dict)
  }
}

class LuceneIndexingProcessorEmbedded(val schemaDef: Config,
                                    val schemaFile: String,
                                    val index: String,
                                    val fieldsMapConf: Seq[Config],
                                    val deleteAll: Boolean,
                                    val optimize: Boolean) {
  val schema: Schema = {
    if (schemaDef != null)
      SchemaLoader.read(schemaDef)
    else if (schemaFile != null)
      SchemaLoader.loadFile(schemaFile)
    else
      throw new IllegalArgumentException("no schema setting found.")
  }

  val fieldsMap: Map[String, String] =
    if (fieldsMapConf != null)
      fieldsMapConf.map(fieldMap => fieldMap.getString("fieldName") -> fieldMap.getString("cellName")).toMap
    else Map()

  def getSchema() = schema

  def getFieldsMap() = fieldsMap

  def write(records: Seq[Record]) = {
    val writer: IWriter = IWriter(index, schema)
    try {
      if (deleteAll) writer.deleteAll()

      val fieldNames = schema.fieldTypes.keySet

      records.foreach {
        record => {
          val fields = new ArrayBuffer[Field]
          fieldNames.foreach {
            fieldName => {
              val cellName = fieldsMap.getOrElse(fieldName, fieldName)
              val value = record.cellValue(cellName)
              if (value != None) {
                fields += Field(fieldName, value.get.toString)
              }
            }
          }
          val doc = new Document(fields.toSet)
          writer.write(doc)
        }
      }
      if (optimize) writer.forceMerge(1)
    }
    finally {
      if (writer != null)
        writer.close()
    }
  }

}
