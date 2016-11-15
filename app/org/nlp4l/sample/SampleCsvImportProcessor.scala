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

package org.nlp4l.sample

import java.io._
import java.nio.charset.Charset

import com.typesafe.config.Config
import org.apache.commons.csv.{CSVFormat, CSVParser, QuoteMode}
import org.nlp4l.framework.models.{Record, _}
import org.nlp4l.framework.processors._

import scala.collection.JavaConversions._
import scala.util.Properties

class SampleCsvImportProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new SampleCsvImportProcessor(
      getStrParamRequired("file"),
      getStrParam("encoding", "UTF-8"),
      getStrListParamRequired("fields")
    )
  }
}

class SampleCsvImportProcessor(val file: String, val encoding: String, val fields: Seq[String]) extends Processor {

  override def execute(data: Option[Dictionary]): Option[Dictionary] = {
    val parser: CSVParser = CSVParser.parse(new File(file), Charset.forName(encoding), CSVFormat.DEFAULT.withHeader(fields: _*).withTrim().withQuoteMode(QuoteMode.MINIMAL))
    try {
      val dict = Dictionary(
        parser.getRecords().map(
          record => Record(
            fields.map(f => Cell(f, record.get(f))))
        )
      )
      Some(dict)
    }
    finally{
      if (parser != null)
        parser.close()
    }
  }
}

class SampleCsvDataProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new SampleCsvDataProcessor(
      getStrListParamRequired("fields"),
      getStrListParamRequired("data")
    )
  }
}

class SampleCsvDataProcessor(val fields: Seq[String], val data: Seq[String]) extends Processor {

  override def execute(dataDict: Option[Dictionary]): Option[Dictionary] = {
    val b = StringBuilder.newBuilder
    data.foreach( d => {
      b.append(d)
      b.append(Properties.lineSeparator)
    })
    val parser: CSVParser = CSVParser.parse(b.toString, CSVFormat.DEFAULT.withHeader(fields: _*).withTrim().withQuoteMode(QuoteMode.MINIMAL))
    try {
      val dict = Dictionary(
        parser.getRecords().map(
          record => Record(
            fields.map(f => Cell(f, record.get(f))))
        )
      )
      Some(dict)
    }
    finally{
      if (parser != null)
        parser.close()
    }
  }
}


