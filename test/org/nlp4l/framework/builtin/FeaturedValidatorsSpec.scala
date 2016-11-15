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

import org.specs2.mutable.Specification

class FeaturedValidatorsSpec extends Specification {

  "UniqueRecordValidator.unique" should {
    "find a value which appeared in multiple items" in {
      val list = "a,b,c,d,e,f,g,b,h,i,j".split(",").toList
      val validator = new UniqueRecordValidator("")
      validator.unique(list) must_== Some("b")
    }

    "not find any values if all values are unique" in {
      val list = "a,b,c,d,e,f,g,h,i,j".split(",").toList
      val validator = new UniqueRecordValidator("")
      validator.unique(list) must_== None
    }
  }

  "RegexValidator.checkPatterns" should {
    "return true if all items are accepted" in {
      val list = "aaa123,bbb123,ccc123,ddd123".split(",")
      val validator = new RegexValidator("", "123", null)
      val result = validator.checkPatterns(list)
      result._1 must_== true
    }

    "return true if all items are not denied" in {
      val list = "aaa123,bbb123,ccc123,ddd123".split(",")
      val validator = new RegexValidator("", null, "456")
      val result = validator.checkPatterns(list)
      result._1 must_== true
    }

    "return true if all items are accepted and not denied" in {
      val list = "aaa123,bbb123,ccc123,ddd123".split(",")
      val validator = new RegexValidator("", "123", "456")
      val result = validator.checkPatterns(list)
      result._1 must_== true
    }

    "return false if one of items is not accepted" in {
      val list = "aaa123,bbb123,ccc456,ddd123".split(",")
      val validator = new RegexValidator("", "123", null)
      val result = validator.checkPatterns(list)
      result._1 must_== false
    }

    "return false if one of items is denied" in {
      val list = "aaa123,bbb456,ccc123,ddd123".split(",")
      val validator = new RegexValidator("", null, "456")
      val result = validator.checkPatterns(list)
      result._1 must_== false
    }

    "return false if all items are accepted but one of them is denied" in {
      val list = "aaa123,bbb123,ccc123,ddd123456".split(",")
      val validator = new RegexValidator("", "123", "456")
      val result = validator.checkPatterns(list)
      result._1 must_== false
    }

    "return false if all items are not denied but one of them is not accepted" in {
      val list = "aaa123,bbb123,ccc123,ddd789".split(",")
      val validator = new RegexValidator("", "123", "456")
      val result = validator.checkPatterns(list)
      result._1 must_== false
    }
  }
}
