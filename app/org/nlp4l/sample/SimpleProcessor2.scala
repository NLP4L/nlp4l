/*
 * Copyright 2015 org.NLP4L
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

import com.typesafe.config.Config
import org.nlp4l.framework.processors._
import org.nlp4l.framework.models._

import scala.collection.mutable.ListBuffer

/**
 * NLP4L framework Processor sample
 * 
 */


class SimpleProcessorFactory2(settings: Config) extends ProcessorFactory(settings) {
  override def getInstance: Processor = {
    new SimpleProcessor2(getStrParam("param1", null), getStrParam("param2", null))
  }
}


class SimpleProcessor2(val param1: String, val param2: String) extends Processor {
  override def execute(data: Option[Dictionary]): Option[Dictionary] = {

    val rcrd01 = Record(Seq(Cell("cell01", "1"), Cell("cell02", 11111), Cell("cell03", null), Cell("cell02_check", null), Cell("cell04", null)))
    val rcrd02 = Record(Seq(Cell("cell01", "1"), Cell("cell02", 22222), Cell("cell03", null), Cell("cell02_check", null), Cell("cell04", null)))
    data match {
      case Some(dic) => {
        Some(dic)
      }
      case None => {
        val ss = Seq(rcrd01, rcrd02)
        val dic = Dictionary(ss)
        Some(dic)
      }
    }
  }
}


