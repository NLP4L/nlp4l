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

package org.nlp4l.ltr.support.procs

import org.nlp4l.ltr.support.models.ImpressionLog


class ClickModelAnalyzer() {
  
  def filterAsDCM(dataICM: Seq[ImpressionLog]) : Seq[ImpressionLog] = {
    dataICM.map(data => {
      if (data.clicks.nonEmpty) {
        val impClicked = data.impressions.map(data.clicks.contains(_))
        val lastClicked = impClicked.lastIndexWhere(_ == true)
        ImpressionLog(data.query, data.impressions.take(lastClicked + 1), data.clicks)
      } else {
        data
      }
    })
  }

  def filterTopQueries(data: Seq[ImpressionLog], topN: Int) : Seq[ImpressionLog] = {
    val qgroup: Map[String, Seq[ImpressionLog]] = data.groupBy(_.query)
    qgroup.toSeq.sortBy(-_._2.size).take(topN).flatMap(_._2)
  }

  def calcClickRate(data: Seq[ImpressionLog]) : Map[String, Map[String, Float]] = {
    val qgroup: Map[String, Seq[ImpressionLog]] = data.groupBy(_.query)
    qgroup.map(qg => {
      val query = qg._1
      val impressions = qg._2.flatMap(_.impressions).groupBy(identity).mapValues(_.size)
      val clicks = qg._2.flatMap(_.clicks).groupBy(identity).mapValues(_.size)
      val clickRates: Map[String, Float] = impressions.map(imp => {
        val docid =  imp._1
        val impCnt = imp._2
        val clickCnt = clicks.getOrElse(docid, 0)
        val clickRate = clickCnt / impCnt.toFloat
        (docid, clickRate)
      })
      (query, clickRates)
    })
  }

  def convertClickRateToLabel(clickRates: Map[String, Map[String, Float]], b: Array[Float]) : Map[String, Map[String, Int]] = {
    clickRates.map(q => {
      val rdslabel = q._2.map( f => {
        val label = b.indexWhere(p => p > f._2)
        if (label == -1)
          (f._1, b.length)
        else
          (f._1, label)
      })
      (q._1, rdslabel)
    })
  }

}

