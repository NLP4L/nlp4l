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

package org.nlp4l.framework.builtin.kea;

import com.typesafe.config.Config;
import org.apache.commons.io.FileUtils;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.Tokenizer;
import org.apache.lucene.analysis.core.LowerCaseFilter;
import org.apache.lucene.analysis.shingle.ShingleFilter;
import org.apache.lucene.analysis.standard.StandardTokenizer;
import org.apache.lucene.analysis.util.CharArraySet;

import java.io.File;
import java.io.IOException;
import java.util.List;


public class KEAStandardAnalyzer extends Analyzer {

  private final int n;
  private final Config params;

  private CharArraySet stopWords = KEAStopWords.defaultStopWords;
  private CharArraySet beginStopWords = KEAStopWords.defaultBeginStopWords;
  private CharArraySet endStopWords = KEAStopWords.defaultEndStopWords;

  public KEAStandardAnalyzer(Integer n, Config params) {
    this.n = n;
    this.params = params;
    if (params != null) {
      if (params.hasPath("stopwords") || params.hasPath("stopwordsFile")) {
        String[] words;
        if (params.hasPath("stopwords")) {
          words = params.getString("stopwords").split(",");
        } else {
          String stopwordsFile = params.getString("stopwordsFile");
          try {
            List<String> lines = FileUtils.readLines(new File(stopwordsFile));
            words = lines.toArray(new String[lines.size()]);
          } catch (IOException e) {
            throw new RuntimeException(e);
          }
        }
        for (int i = 0; i < words.length; i++) {
          words[i] = words[i].trim();
        }
        CharArraySet[] set = KEAStopWords.createStopWordsSet(words);
        stopWords = set[0];
        beginStopWords = set[1];
        endStopWords = set[2];
      }
    }
  }

  @Override
  protected TokenStreamComponents createComponents(String fieldName) {
    Tokenizer source = new StandardTokenizer();
    TokenStream lcf = new LowerCaseFilter(source);
    if (n == 1) {
      TokenStream stf = new KEAStopFilter(lcf, n, stopWords, beginStopWords, endStopWords);
      return new TokenStreamComponents(source, stf);
    } else {
      assert n >= 2;
      ShingleFilter shf = new ShingleFilter(lcf, n, n);
      shf.setOutputUnigrams(false);
      KEAStopFilter keasf = new KEAStopFilter(shf, n, stopWords, beginStopWords, endStopWords);
      return new TokenStreamComponents(source, keasf);
    }
  }
}
