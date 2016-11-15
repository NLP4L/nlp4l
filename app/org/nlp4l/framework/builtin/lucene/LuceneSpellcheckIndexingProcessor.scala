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

import java.io.FileInputStream
import java.nio.file.FileSystems

import com.typesafe.config.Config
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.{IndexWriterConfig, DirectoryReader}
import org.apache.lucene.search.spell.{Dictionary => SpellDictionary, PlainTextDictionary, LuceneDictionary, SpellChecker}
import org.apache.lucene.search.suggest.FileDictionary
import org.apache.lucene.store.FSDirectory
import org.nlp4l.framework.models.{Cell, Record, Dictionary}
import org.nlp4l.framework.processors._

class LuceneSpellcheckIndexingProcessorFactory(settings: Config) extends ProcessorFactory(settings) {
  override def getInstance(): Processor = {
    new LuceneSpellcheckIndexingProcessor(
      getStrParamRequired("index"), getStrParamRequired("dicType"), getConfigParam("settings", null))
  }
}

class LuceneSpellcheckIndexingProcessor(val index: String, val dicType: String, val settings: Config) extends Processor {

  val dictionary: SpellDictionary = dicType match {
    case "plainText" => {
      val file = settings.getString("file")
      new PlainTextDictionary(FileSystems.getDefault.getPath(file))
    }
    case "file" => {
      val file = settings.getString("file")
      val delimiter = if(settings.hasPath("delimiter")) settings.getString("delimiter")
                      else FileDictionary.DEFAULT_FIELD_DELIMITER
      val is = new FileInputStream(file)
      new FileDictionary(is, delimiter)
    }
    case "lucene" => {
      val idxDir = FSDirectory.open(FileSystems.getDefault.getPath(settings.getString("index")))
      val field = settings.getString("field")
      val reader = DirectoryReader.open(idxDir)
      new LuceneDictionary(reader, field)
    }
    case a => throw new IllegalArgumentException(s"unknown dictionary type was specified: $a")
  }
  val spellChecker: SpellChecker = new SpellChecker(FSDirectory.open(FileSystems.getDefault.getPath(index)))

  override def execute(data: Option[Dictionary]): Option[Dictionary] = {
    spellChecker.indexDictionary(dictionary, new IndexWriterConfig(new StandardAnalyzer()), true)
    spellChecker.close

    val dict = Dictionary(Seq(
      Record(Seq(
        Cell("result", s"success: spellcheck index '$index' has been created.")
      ))))
    Some(dict)
  }
}
