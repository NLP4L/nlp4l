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

package org.nlp4l.framework.builtin

import java.io.{BufferedWriter, File, FileWriter, PrintWriter, _}

import com.typesafe.config.Config
import org.nlp4l.framework.builtin.DbModels.fWRecordWithAttrubuteWrites
import org.nlp4l.framework.models.{Dictionary, DictionaryAttribute, Record, RecordWithAttrbute}
import org.nlp4l.framework.processors.{Writer, WriterFactory}
import play.api.libs.json.Json
import resource._


class JsonFileWriterFactory(settings: Config) extends WriterFactory(settings) {

  override def getInstance: Writer = {
    new JsonFileWriter(
      getStrParam("file", null)
    )
  }
}

class JsonFileWriter(file: String) extends Writer {

  override def write(data: Option[Dictionary], dictionaryAttribute: DictionaryAttribute): String = {
    data match {
      case Some(dic) => {
        val outfile = if (file == null) File.createTempFile("nlp4l-", ".json") else new File(file)
        val result = dic.recordList.map { x: Record => RecordWithAttrbute(x, dictionaryAttribute) }
        val jsValue = Json.toJson(result)
        for (output <- managed(new PrintWriter(new BufferedWriter(new FileWriter(outfile, false))))) {
          output.print(Json.prettyPrint(jsValue))
        }
        outfile.getAbsolutePath
      }
      case None => throw new Exception("no data to write found")
    }
  }
}

class CSVFileWriterFactory(settings: Config) extends WriterFactory(settings) {

  override def getInstance: Writer = {
    new CSVFileWriter(
      getStrParam("file", null),
      getStrParam("separator", ","),
      getStrParam("encoding", "UTF-8")
    )
  }
}

class CSVFileWriter(file: String, separator: String, encoding: String) extends Writer {

  override def write(data: Option[Dictionary], dictionaryAttribute: DictionaryAttribute): String = {
    data match {
      case Some(dic) => {
        val outfile = if (file == null) File.createTempFile("nlp4l-", ".csv") else new File(file)
        val result = dic.recordList.map(r => r.mkCsvRecord(separator))

        for (output <- managed(new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outfile), encoding))))) {
          result.foreach(output.println(_))
        }
        outfile.getAbsolutePath
      }
      case None => throw new Exception("no data to write found")
    }
  }
}

