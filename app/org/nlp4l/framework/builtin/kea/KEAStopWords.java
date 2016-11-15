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

import org.apache.lucene.analysis.util.CharArraySet;


public class KEAStopWords {

  // feel free to add your stop words to this list if needed
  static final String STOP_WORDS = "a,an,and,are,as,at,be,but,by,for,if,in,into,is,it,no,not,of,on,or,such,that,the,their,then,there,these,they,this,to,was,will,with" // using Lucene's stop words
    + ",after,although,because,before,even,how,lest,once,since,than,though,till,unless,until,when,what,whatever,whenever,where,whereas,wherever,whether,which,whichever,while,who,whose,why" // conjunctions
    + ",about,above,across,ago,below,beside,besides,from,off,onto,over,past,through,toward,towards,under" // prepositions
    + ",he,her,hers,herself,him,himself,his,i,its,itself,me,mine,my,myself,our,ours,ourselves,she,theirs,them,themselves,us,we,you,your,yours,yourself,yourselves" // pronouns
    + ",been,became,become,began,begin,begun,came,come,coming,did,didn't,do,don't,does,doesn't,doing,done,gave,get,getting,give,given,go,gone,got,gotten,had,have,keep,kept,knew,know,known,leave,left,let,went,were" // anomalous verbs
    + ",can,cannot,can't,could,couldn't,made,make,ought,said,say,send,sent,shall,should,shouldn't,take,taken,tell,think,thought,told,took,will,would,wouldn't";

  public static final CharArraySet defaultStopWords = new CharArraySet(STOP_WORDS.length(), true);
  public static final CharArraySet defaultBeginStopWords = new CharArraySet(STOP_WORDS.length(), true);
  public static final CharArraySet defaultEndStopWords = new CharArraySet(STOP_WORDS.length(), true);

  static {
    String[] words = STOP_WORDS.split(",");
    for (String word : words) {
      defaultStopWords.add(word);
      defaultBeginStopWords.add(word + " ");
      defaultEndStopWords.add(" " + word);
    }
  }
  public static CharArraySet[] createStopWordsSet(String[] words) {
    CharArraySet[] set = new CharArraySet[3];
    set[0] = new CharArraySet(words.length, true);
    set[1] = new CharArraySet(words.length, true);
    set[2] = new CharArraySet(words.length, true);
    for (String word : words) {
      set[0].add(word);
      set[1].add(word + " ");
      set[2].add(" " + word);
    }
    return set;
  }

}

