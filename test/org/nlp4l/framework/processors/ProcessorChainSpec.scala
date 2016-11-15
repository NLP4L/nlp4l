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

package org.nlp4l.framework.processors

import org.nlp4l.framework.models.{Dictionary, Cell, Record}
import org.specs2.mutable.Specification

class ProcessorChainSpec extends Specification {

  class ARecordProcessor(val multiply: Int, val skip: (Int) => Boolean = null) extends RecordProcessor {
    override def execute(data: Option[Record]): Option[Record] = {
      val result = data.get.cellList(0).value.asInstanceOf[Int] * multiply
      if(skip == null) Some(Record(Seq(Cell("value", result))))
      else if(skip(result)) None
      else Some(Record(Seq(Cell("value", result))))
    }
  }

  "WrapProcessor" should {
    "process multiple RecordProcessors" in {
      val rec1Proc = new ARecordProcessor(2)
      val rec2Proc = new ARecordProcessor(3)
      val wrapProc = new WrapProcessor(Seq(rec1Proc, rec2Proc))
      val inputs = Some(Dictionary(Seq(
        Record(Seq(Cell("val", 1))),
        Record(Seq(Cell("val", 2))),
        Record(Seq(Cell("val", 3)))
      )))
      val result = wrapProc.execute(inputs)
      result mustNotEqual None
      val records = result.get.recordList
      records.length mustEqual 3
      records(0).cellList(0).value.asInstanceOf[Int] mustEqual 6 // 1 * 2 * 3
      records(1).cellList(0).value.asInstanceOf[Int] mustEqual 12 // 2 * 2 * 3
      records(2).cellList(0).value.asInstanceOf[Int] mustEqual 18 // 3 * 2 * 3
    }

    "skip processing for empty Records" in {
      val rec1Proc = new ARecordProcessor(2, _ == 4)
      val rec2Proc = new ARecordProcessor(3)
      val wrapProc = new WrapProcessor(Seq(rec1Proc, rec2Proc))
      val inputs = Some(Dictionary(Seq(
        Record(Seq(Cell("val", 1))),
        Record(Seq(Cell("val", 2))),
        Record(Seq(Cell("val", 3)))
      )))
      val result = wrapProc.execute(inputs)
      result mustNotEqual None
      val records = result.get.recordList
      records.length mustEqual 2
      records(0).cellList(0).value.asInstanceOf[Int] mustEqual 6 // 1 * 2 * 3
      records(1).cellList(0).value.asInstanceOf[Int] mustEqual 18 // 3 * 2 * 3
    }
  }
}
