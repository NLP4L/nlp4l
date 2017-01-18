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

package org.nlp4l.ltr.support.procs

import org.nlp4l.ltr.support.models.ImpressionLog
import org.nlp4l.ltr.support.models.ClickModels._
import org.specs2.mutable.Specification
import play.api.libs.json.{JsValue, Json}

class ClickModelAnalyzerSpec extends Specification {

  "ClickModelAnalyzer" should {

    val JSON_TEXT = s"""
       |{
       | "data": [
       |   {
       |     "query": "iPhone",
       |     "impressions": [ "docA", "docB", "docC", "docD", "docE" ],
       |     "clicks": [ "docA", "docC" ]
       |   },
       |   {
       |     "query": "iPhone",
       |     "impressions": [ "docA", "docB", "docC", "docD", "docE" ],
       |     "clicks": [ "docA" ]
       |   },
       |   {
       |     "query": "Android",
       |     "impressions": [ "docA", "docB", "docC", "docD", "docE" ],
       |     "clicks": [ "docB" ]
       |   },
       |   {
       |     "query": "Android",
       |     "impressions": [ "docA", "docB", "docC", "docD", "docE" ],
       |     "clicks": []
       |   }
       | ]
       |}
        """.stripMargin

    "analyze ICM" in {

      val json: JsValue = Json.parse(JSON_TEXT)
      val data =  (json \ "data").as[Seq[ImpressionLog]]
      val clickModel = new ClickModelAnalyzer()
      val clickRates = clickModel.calcClickRate(data)

      println("ICM: " + clickRates)

      clickRates.size mustEqual 2
      clickRates.get("iPhone").get.size mustEqual 5
      clickRates.get("iPhone").get.get("docA").get mustEqual 1.0
      clickRates.get("iPhone").get.get("docB").get mustEqual 0
      clickRates.get("iPhone").get.get("docC").get mustEqual 0.5
      clickRates.get("iPhone").get.get("docD").get mustEqual 0
      clickRates.get("iPhone").get.get("docE").get mustEqual 0
      clickRates.get("Android").get.size mustEqual 5
      clickRates.get("Android").get.get("docA").get mustEqual 0
      clickRates.get("Android").get.get("docB").get mustEqual 0.5
      clickRates.get("Android").get.get("docC").get mustEqual 0
      clickRates.get("Android").get.get("docD").get mustEqual 0
      clickRates.get("Android").get.get("docE").get mustEqual 0

      val b = Array(0.01, 0.2, 0.5, 0.6, 0.8).map(_.toFloat)
      val clickRateLabels = clickModel.convertClickRateToLabel(clickRates, b)

      println("ICM: " + clickRateLabels)

      clickRateLabels.size mustEqual 2
      clickRateLabels.get("iPhone").get.size mustEqual 5
      clickRateLabels.get("iPhone").get.get("docA").get mustEqual 5
      clickRateLabels.get("iPhone").get.get("docB").get mustEqual 0
      clickRateLabels.get("iPhone").get.get("docC").get mustEqual 3
      clickRateLabels.get("iPhone").get.get("docD").get mustEqual 0
      clickRateLabels.get("iPhone").get.get("docE").get mustEqual 0
      clickRateLabels.get("Android").get.size mustEqual 5
      clickRateLabels.get("Android").get.get("docA").get mustEqual 0
      clickRateLabels.get("Android").get.get("docB").get mustEqual 3
      clickRateLabels.get("Android").get.get("docC").get mustEqual 0
      clickRateLabels.get("Android").get.get("docD").get mustEqual 0
      clickRateLabels.get("Android").get.get("docE").get mustEqual 0
    }

    "analyze DCM" in {

      val json: JsValue = Json.parse(JSON_TEXT)
      val data =  (json \ "data").as[Seq[ImpressionLog]]
      val clickModel = new ClickModelAnalyzer()
      val dataDCM = clickModel.filterAsDCM(data)
      val clickRates = clickModel.calcClickRate(dataDCM)

      println("DCM: " + clickRates)

      clickRates.size mustEqual 2
      clickRates.get("iPhone").get.size mustEqual 3
      clickRates.get("iPhone").get.get("docA").get mustEqual 1.0
      clickRates.get("iPhone").get.get("docB").get mustEqual 0
      clickRates.get("iPhone").get.get("docC").get mustEqual 1.0
      clickRates.get("Android").get.size mustEqual 5
      clickRates.get("Android").get.get("docA").get mustEqual 0
      clickRates.get("Android").get.get("docB").get mustEqual 0.5
      clickRates.get("Android").get.get("docC").get mustEqual 0
      clickRates.get("Android").get.get("docD").get mustEqual 0
      clickRates.get("Android").get.get("docE").get mustEqual 0

      val b = Array(0.01, 0.2, 0.5, 0.6, 0.8).map(_.toFloat)
      val clickRateLabels = clickModel.convertClickRateToLabel(clickRates, b)

      println("DCM: " + clickRateLabels)

      clickRateLabels.size mustEqual 2
      clickRateLabels.get("iPhone").get.size mustEqual 3
      clickRateLabels.get("iPhone").get.get("docA").get mustEqual 5
      clickRateLabels.get("iPhone").get.get("docB").get mustEqual 0
      clickRateLabels.get("iPhone").get.get("docC").get mustEqual 5
      clickRateLabels.get("Android").get.size mustEqual 5
      clickRateLabels.get("Android").get.get("docA").get mustEqual 0
      clickRateLabels.get("Android").get.get("docB").get mustEqual 3
      clickRateLabels.get("Android").get.get("docC").get mustEqual 0
      clickRateLabels.get("Android").get.get("docD").get mustEqual 0
      clickRateLabels.get("Android").get.get("docE").get mustEqual 0
    }

    "analyze Multiple Clicks" in {

      val JSON_TEXT2 = s"""
      |{
      | "data": [
      |   {
      |     "query": "iPhone",
      |     "impressions": [ "docA", "docB", "docC", "docD", "docE" ],
      |     "clicks": [ "docA", "docC", "docA" ]
      |   }
      | ]
      |}
        """.stripMargin
      val json: JsValue = Json.parse(JSON_TEXT2)
      val data =  (json \ "data").as[Seq[ImpressionLog]]
      val clickModel = new ClickModelAnalyzer()
      val clickRates = clickModel.calcClickRate(data)

      println("Multi-Clicks: " + clickRates)

      clickRates.get("iPhone").get.size mustEqual 5
      clickRates.get("iPhone").get.get("docA").get mustEqual 2.0
      clickRates.get("iPhone").get.get("docB").get mustEqual 0
      clickRates.get("iPhone").get.get("docC").get mustEqual 1.0
      clickRates.get("iPhone").get.get("docD").get mustEqual 0
      clickRates.get("iPhone").get.get("docE").get mustEqual 0

      val b = Array(0.01, 0.2, 0.5, 0.6, 0.8).map(_.toFloat)
      val clickRateLabels = clickModel.convertClickRateToLabel(clickRates, b)

      println("Multi-Clicks: " + clickRateLabels)

      clickRateLabels.get("iPhone").get.size mustEqual 5
      clickRateLabels.get("iPhone").get.get("docA").get mustEqual 5
      clickRateLabels.get("iPhone").get.get("docB").get mustEqual 0
      clickRateLabels.get("iPhone").get.get("docC").get mustEqual 5
      clickRateLabels.get("iPhone").get.get("docD").get mustEqual 0
      clickRateLabels.get("iPhone").get.get("docE").get mustEqual 0
    }

    "filter Top Queries" in {

      val JSON_TEXT2 = s"""
       |{
       | "data": [
       |   { "query": "iPhone", "impressions": [], "clicks": []},
       |   { "query": "iPhone", "impressions": [], "clicks": []},
       |   { "query": "BlackBerry", "impressions": [], "clicks": []},
       |   { "query": "Android", "impressions": [], "clicks": []},
       |   { "query": "Android", "impressions": [], "clicks": []},
       |   { "query": "Android", "impressions": [], "clicks": []}
       | ]
       |}
        """.stripMargin
      val json: JsValue = Json.parse(JSON_TEXT2)
      val data =  (json \ "data").as[Seq[ImpressionLog]]
      val clickModel = new ClickModelAnalyzer()
      val dataTopN = clickModel.filterTopQueries(data, 2)

      val qgroup: Map[String, Seq[ImpressionLog]] = dataTopN.groupBy(_.query)

      println("TopN: " + qgroup)

      qgroup.size mustEqual 2
      qgroup.get("iPhone").get.size mustEqual 2
      qgroup.get("Android").get.size mustEqual 3

    }

  }
}
