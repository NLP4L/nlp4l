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

import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute;
import org.apache.lucene.analysis.util.CharArraySet;
import org.apache.lucene.analysis.util.FilteringTokenFilter;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class KEAStopFilter extends FilteringTokenFilter {

  private final int n;
  private final CharArraySet stopWords, beginStopWords, endStopWords;
  private final CharTermAttribute termAtt = addAttribute(CharTermAttribute.class);

  public static Pattern patNumbers = Pattern.compile("^\\d+$");

  public KEAStopFilter(TokenStream in, int n, CharArraySet stopWords, CharArraySet beginStopWords, CharArraySet endStopWords) {
    super(in);
    this.n = n;
    this.stopWords = stopWords;
    this.beginStopWords = beginStopWords;
    this.endStopWords = endStopWords;
  }

  @Override
  protected boolean accept() throws IOException {
    String phrase = termAtt.toString();
    if (n == 1) {
      if (stopWords.contains(phrase)) return false;

      if (phrase.length() == 1) return false;

      Matcher m = patNumbers.matcher(phrase);
      return !m.find();
    } else {
      assert n >= 2;
      String words[] = phrase.split(" ");
      if (beginStopWords.contains(words[0] + " ")) return false;
      else if (endStopWords.contains(" " + words[words.length - 1])) return false;
      return true;
    }
  }
}
