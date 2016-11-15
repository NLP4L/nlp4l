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

import com.typesafe.config.ConfigFactory
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.analysis.{Analyzer, TokenStream}
import org.specs2.mutable.Specification

class KEAStandardAnalyzerSpec extends Specification {

  "KEAStandardAnalyzer" should {
    "analyze n=1 " in {
      val analyzer = new KEAStandardAnalyzer(1, null);
      val text = "AAA of BBB is CCC DDD"
      val tokens = getTokens(analyzer, text)
      tokens.length mustEqual 4
      tokens(0) mustEqual "aaa"
      tokens(1) mustEqual "bbb"
      tokens(2) mustEqual "ccc"
      tokens(3) mustEqual "ddd"
    }
    "analyze n=2 " in {
      val analyzer = new KEAStandardAnalyzer(2, null);
      val text = "AAA of BBB is CCC DDD"
      val tokens = getTokens(analyzer, text)
      tokens.length mustEqual 1
      tokens(0) mustEqual "ccc ddd"
    }
    "analyze n=3 " in {
      val analyzer = new KEAStandardAnalyzer(3, null);
      val text = "AAA of BBB is CCC DDD"
      val tokens = getTokens(analyzer, text)
      tokens.length mustEqual 2
      tokens(0) mustEqual "aaa of bbb"
      tokens(1) mustEqual "bbb is ccc"
    }
    "analyze with stopwords " in {
      val params = ConfigFactory.parseString("{ stopwords: \"aaa, ccc\" }");
      val analyzer = new KEAStandardAnalyzer(1, params);
      val text = "AAA BBB CCC DDD"
      val tokens = getTokens(analyzer, text)
      tokens.length mustEqual 2
      tokens(0) mustEqual "bbb"
      tokens(1) mustEqual "ddd"
    }
    //    "analyze with stopwordsFile " in {
    //      new PrintWriter("/tmp/kea-stopwords.txt") { println("bbb"); println("ccc"); close }
    //      val params = ConfigFactory.parseString("{ stopwordsFile: /tmp/kea-stopwords.txt }");
    //      val analyzer = new KEAStandardAnalyzer(1, params);
    //      val text = "AAA BBB CCC DDD"
    //      val tokens = getTokens(analyzer, text)
    //      tokens.length mustEqual 2
    //      tokens(0) mustEqual "aaa"
    //      tokens(1) mustEqual "ddd"
    //    }
  }

  def getTokens(analyzer: Analyzer, text: String): Seq[String] = {
    val stream: TokenStream = analyzer.tokenStream("dummy", text)
    val charTermAttr = stream.addAttribute(classOf[CharTermAttribute])
    stream.reset()
    try {
      Iterator
        .continually(stream)
        .takeWhile(_.incrementToken())
        .map(_ => charTermAttr.toString)
        .toVector
    }
    finally {
      stream.end()
      stream.close()
    }
  }

}
