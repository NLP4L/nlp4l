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

package org.nlp4l.framework.builtin.kea

import java.io._
import java.nio.file.FileSystems
import java.util

import com.typesafe.config.Config
import org.apache.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.analysis.util.CharArraySet
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.apache.lucene.document._
import org.apache.lucene.index.{IndexOptions, IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.{Directory, FSDirectory}
import org.nlp4l.framework.models.Dictionary
import org.nlp4l.lucene.SchemaLoader

import scala.util.Try
import scalax.file.Path

trait CommonProcessor {

  val CONTENT_ID_FIELD_NAME = "id"
  val FIELD_NAME = "content"
  val DOC_SIZE_FIELD_NAME = "size"

  def indexing(index: String, textField: String, data: Option[Dictionary], analyzerConfig: Config, documentSizeAnalyzerConfig: Config, withTermVectors: Boolean): Unit = {
    val path = Path.fromString(index)
    Try(path.deleteRecursively(continueOnFailure = false))

    val indexDir: Directory = FSDirectory.open(FileSystems.getDefault.getPath(index))
    val analyzer: Analyzer = getKEAAnalyzer(FIELD_NAME, analyzerConfig)
    val documentSizeAnalyzer: Analyzer =
      if (documentSizeAnalyzerConfig != null)
        SchemaLoader.readAnalyzer(documentSizeAnalyzerConfig).delegate
      else
        new StandardAnalyzer(CharArraySet.EMPTY_SET)

    val iwc: IndexWriterConfig = new IndexWriterConfig(analyzer)
    val iw: IndexWriter = new IndexWriter(indexDir, iwc)
    try {
      data.get.recordList.zipWithIndex.foreach {
        case (record, i) =>
          val text = record.cellValue(textField).get.toString
          val size = getDocumentSize(text, documentSizeAnalyzer)
          if (withTermVectors)
            iw.addDocument(getDocumentWithTermVectors(i.toString, text, size))
          else
            iw.addDocument(getDocument(i.toString, text, size))
      }
      iw.forceMerge(1, true)
    }
    finally {
      if (iw != null)
        iw.close()
    }
  }

  def getFieldName(baseNamem: String, n: Int): String = {
    baseNamem + "_" + String.valueOf(n)
  }

  def getKEAAnalyzer(fieldName: String, analyzerConfig: Config): Analyzer = {
    val className = if (analyzerConfig != null && analyzerConfig.hasPath("class")) analyzerConfig.getString("class") else "org.nlp4l.framework.builtin.kea.KEAStandardAnalyzer"
    val params = if (analyzerConfig != null && analyzerConfig.hasPath("params")) analyzerConfig.getConfig("params") else null
    val constructor = Class.forName(className).getConstructor(classOf[Integer], classOf[Config])
    val amap = new util.HashMap[String, Analyzer]
    val a1 = constructor.newInstance(new Integer(1), params).asInstanceOf[Analyzer]
    val a2 = constructor.newInstance(new Integer(2), params).asInstanceOf[Analyzer]
    val a3 = constructor.newInstance(new Integer(3), params).asInstanceOf[Analyzer]
    amap.put(getFieldName(fieldName, 1), a1)
    amap.put(getFieldName(fieldName, 2), a2)
    amap.put(getFieldName(fieldName, 3), a3)
    new PerFieldAnalyzerWrapper(new StandardAnalyzer, amap)
  }

  def getDocument(contentId: String, content: String, docSize: Int): Document = {
    val doc: Document = new Document
    doc.add(new StringField(CONTENT_ID_FIELD_NAME, contentId, Field.Store.YES))
    doc.add(new StoredField(DOC_SIZE_FIELD_NAME, docSize))
    doc.add(new TextField(getFieldName(FIELD_NAME, 1), content, Field.Store.YES))
    doc.add(new TextField(getFieldName(FIELD_NAME, 2), content, Field.Store.YES))
    doc.add(new TextField(getFieldName(FIELD_NAME, 3), content, Field.Store.YES))
    doc
  }

  def getDocumentWithTermVectors(contentId: String, content: String, docSize: Int): Document = {
    val doc: Document = new Document
    doc.add(new StringField(CONTENT_ID_FIELD_NAME, contentId, Field.Store.YES))
    doc.add(new StoredField(DOC_SIZE_FIELD_NAME, docSize))
    val ft: FieldType = new FieldType
    ft.setStored(true)
    ft.setIndexOptions(IndexOptions.DOCS_AND_FREQS_AND_POSITIONS)
    ft.setStoreTermVectors(true)
    doc.add(new Field(getFieldName(FIELD_NAME, 1), content, ft))
    doc.add(new Field(getFieldName(FIELD_NAME, 2), content, ft))
    doc.add(new Field(getFieldName(FIELD_NAME, 3), content, ft))
    doc
  }

  def getDocumentSize(text: String, analyzer: Analyzer): Int = {
    val stream: TokenStream = analyzer.tokenStream("dummy", text)
    stream.reset
    try {
      Iterator
        .continually(stream)
        .takeWhile(_.incrementToken())
        .size
    }
    finally {
      stream.end
      stream.close
    }
  }

  def log2(num: Double): Double = {
    Math.log(num) / Math.log(2.0)
  }

  def calcTfIdf(freq: Int, docSize: Int, docFreq: Int, docNum: Int): Double = {
    freq.toDouble / docSize.toDouble * (-1) * log2(docFreq.toDouble / docNum.toDouble)
  }

  def calcFirstOccurrence(pos: Int, docSize: Int): Double = {
    pos.toDouble / docSize.toDouble
  }
}


class ModelDirSettings(modelDir: String) {

  val LUCENE_INDEX_MODEL_DIR = new File(modelDir, "lucene-index-model").getAbsolutePath
  val LUCENE_INDEX_EXTRACT_DIR = new File(modelDir, "lucene-index-extract").getAbsolutePath

  val FEATURES_MODEL_FILE = new File(modelDir, "features-model.txt").getAbsolutePath
  val KEYPHRASES_MODEL_FILE = new File(modelDir, "keyphrases-model.txt").getAbsolutePath
  val CUTP_MODEL_FILE = new File(modelDir, "cutp-model.txt").getAbsolutePath

}





