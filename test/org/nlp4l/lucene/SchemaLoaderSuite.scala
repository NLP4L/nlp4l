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

package org.nlp4l.lucene

import org.apache.lucene.analysis.core.StopFilterFactory
import org.apache.lucene.analysis.custom.CustomAnalyzer
import org.scalatest.FunSuite

class SchemaLoaderSuite extends FunSuite {

  test("load valid schema") {
    val schema = SchemaLoader.load("org/nlp4l/lucene/testschema1.conf")
    assertResult("org.apache.lucene.analysis.core.KeywordAnalyzer")(schema.defaultAnalyzer.delegate.getClass.getName)

    assertResult(3)(schema.fieldTypes.size)

    assertResult(null)(schema.fieldTypes.get("fieldA").get.analyzer)
    assertResult(false)(schema.fieldTypes.get("fieldA").get.indexed)
    assertResult(false)(schema.fieldTypes.get("fieldA").get.stored)
    assertResult(false)(schema.fieldTypes.get("fieldA").get.termVectors)
    assertResult(false)(schema.fieldTypes.get("fieldA").get.termPositions)
    assertResult(false)(schema.fieldTypes.get("fieldA").get.termOffsets)


    assertResult("org.apache.lucene.analysis.custom.CustomAnalyzer")(schema.fieldTypes("fieldB").analyzer.delegate.getClass.getName)
    val myAnalyzer1 = schema.fieldTypes("fieldB").analyzer.delegate.asInstanceOf[CustomAnalyzer]
    assertResult("org.apache.lucene.analysis.standard.StandardTokenizerFactory")(myAnalyzer1.getTokenizerFactory.getClass.getName)
    assertResult("org.apache.lucene.analysis.core.StopFilterFactory")(myAnalyzer1.getTokenFilterFactories.get(0).getClass.getName)
    val stop = myAnalyzer1.getTokenFilterFactories.get(0).asInstanceOf[StopFilterFactory]
    assertResult(true)(stop.isIgnoreCase)
    assertResult(33)(stop.getStopWords.size)
    assertResult("org.apache.lucene.analysis.core.LowerCaseFilterFactory")(myAnalyzer1.getTokenFilterFactories.get(1).getClass.getName)
    assertResult(true)(schema.fieldTypes.get("fieldB").get.indexed)
    assertResult(true)(schema.fieldTypes.get("fieldB").get.stored)
    assertResult(true)(schema.fieldTypes.get("fieldB").get.termVectors)
    assertResult(true)(schema.fieldTypes.get("fieldB").get.termPositions)
    assertResult(true)(schema.fieldTypes.get("fieldB").get.termOffsets)

    assertResult("org.apache.lucene.analysis.custom.CustomAnalyzer")(schema.fieldTypes("fieldC").analyzer.delegate.getClass.getName)
    val myAnalyzer2 = schema.fieldTypes("fieldC").analyzer.delegate.asInstanceOf[CustomAnalyzer]
    assertResult("org.apache.lucene.analysis.ngram.NGramTokenizerFactory")(myAnalyzer2.getTokenizerFactory.getClass.getName)
  }

  test("except InvalidSchemaException when root path not found") {
    intercept[InvalidSchemaException] {
      val schema = SchemaLoader.load("org/nlp4l/lucene/testschema_invalid1.conf")
    }
  }

  test("except InvalidSchemaException when the path schema.defAnalyzer not found") {
    intercept[InvalidSchemaException] {
      val schema = SchemaLoader.load("org/nlp4l/lucene/testschema_invalid2.conf")
    }
  }

  test("except InvalidSchemaException when the path schema.fields not found") {
    intercept[InvalidSchemaException] {
      val schema = SchemaLoader.load("org/nlp4l/lucene/testschema_invalid3.conf")
    }
  }

  test("except InvalidSchemaException when schema.fields contains no elements") {
    intercept[InvalidSchemaException] {
      val schema = SchemaLoader.load("org/nlp4l/lucene/testschema_invalid4.conf")
    }
  }

  test("except InvalidSchemaException when the path schema.fields.[N].name not found") {
    intercept[InvalidSchemaException] {
      val schema = SchemaLoader.load("org/nlp4l/lucene/testschema_invalid5.conf")
    }
  }
}
