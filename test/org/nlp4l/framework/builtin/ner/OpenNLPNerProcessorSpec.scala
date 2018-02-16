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

package org.nlp4l.framework.builtin.ner

import java.io._
import java.net.URL
import java.security.cert.X509Certificate
import javax.net.ssl.{HttpsURLConnection, SSLContext, X509TrustManager}

import com.typesafe.config.ConfigFactory
import org.nlp4l.framework.models.{Cell, Record}
import org.specs2.mutable.{BeforeAfter, Specification}
import dispatch._

class OpenNLPNerProcessorSpec extends Specification with BeforeAfter {

  object OpenNLPModels {
    val DIR = System.getProperty("java.io.tmpdir")
    val FILE_SENT = "en-sent.bin"
    val FILE_TOKEN = "en-token.bin"
    val FILE_NER_PERSON = "en-ner-person.bin"
    val FILE_NER_LOCATION = "en-ner-location.bin"

    def sentModel(): String = { new File(DIR, FILE_SENT).getAbsolutePath.replace('\\', '/') }
    def tokenModel(): String = { new File(DIR, FILE_TOKEN).getAbsolutePath.replace('\\', '/') }
    def nerPersonModel(): String = { new File(DIR, FILE_NER_PERSON).getAbsolutePath.replace('\\', '/') }
    def nerLocationModel(): String = { new File(DIR, FILE_NER_LOCATION).getAbsolutePath.replace('\\', '/') }
  }

  def before = {
    downloadOpenNLPModel(OpenNLPModels.FILE_SENT)
    downloadOpenNLPModel(OpenNLPModels.FILE_TOKEN)
    downloadOpenNLPModel(OpenNLPModels.FILE_NER_PERSON)
    downloadOpenNLPModel(OpenNLPModels.FILE_NER_LOCATION)
  }

  def downloadOpenNLPModel(file: String): Unit = {
    val modelFile = new File(OpenNLPModels.DIR, file)
    val trustManager = new X509TrustManager {
      override def getAcceptedIssuers: Array[X509Certificate] = null
      override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}
      override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}
    }
    val sc = SSLContext.getInstance("SSL")
    sc.init(null, Array(trustManager), new java.security.SecureRandom())
    HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory())

    if(!modelFile.exists()){
      val downloadUrl = new URL(s"https://www.rondhuit.com/download/opennlp-models-1.5/$file")
      val connection = downloadUrl.openConnection()

      var in = None: Option[InputStream]
      var out = None: Option[FileOutputStream]
      try {
        in = Some(connection.getInputStream)
        out = Some(new FileOutputStream(modelFile))
        var c = in.get.read
        while (c >= 0) {
          out.get.write(c)
          c = in.get.read
        }
      } catch {
        case e: IOException => e.printStackTrace
      } finally {
        if (in.isDefined) in.get.close
        if (out.isDefined) out.get.close
      }
    }
  }

  def after = {
  }

  "OpenNLPNerRecordProcessor" should {
    "execute extraction" in {
      val config = ConfigFactory.parseString(
        s"""
          |{
          | sentModel: "${OpenNLPModels.sentModel}"
          | tokenModel: "${OpenNLPModels.tokenModel}"
          | nerModels: [
          |   "${OpenNLPModels.nerPersonModel}",
          |   "${OpenNLPModels.nerLocationModel}"
          |   ]
          | nerTypes: [
          |   "person",
          |   "location"
          |   ]
          | srcFields: [
          |   "body",
          |   "title"
          |   ]
          | idField: docId
          | passThruFields: [
          |   "category"
          |   ]
          |}
        """.stripMargin)

      val proc = new OpenNLPNerRecordProcessorFactory(config).getInstance()

      val record = Record(Seq(
          Cell("docId", "DOC-001"),
          Cell("category", "Sports"),
          Cell("title", "Mark mets Chris at Boston"),
          Cell("body", "Mr. Mark Warburton and Mr. Chris Heston are in Boston and Los Angeles."))
      )
        val result = proc.execute(Some(record))

        result.get.cellList.length must_==(6)

        result.get.cellList(0).name mustEqual "docId"
        result.get.cellList(0).value mustEqual "DOC-001"

        result.get.cellList(1).name mustEqual "body_person"
        result.get.cellList(1).value mustEqual "Mark Warburton,Chris Heston"

        result.get.cellList(2).name mustEqual "body_location"
        result.get.cellList(2).value mustEqual "Boston,Los Angeles"

        result.get.cellList(3).name mustEqual "title_person"
        result.get.cellList(3).value mustEqual "Mark,Chris"

        result.get.cellList(4).name mustEqual "title_location"
        result.get.cellList(4).value mustEqual "Boston"

        result.get.cellList(5).name mustEqual "category"
        result.get.cellList(5).value mustEqual "Sports"
      }
  }
  "OpenNLPNerRecordProcessor" should {
    "execute extraction, with separator, without passThru" in {
      val config = ConfigFactory.parseString(
        s"""
          |{
          | sentModel: "${OpenNLPModels.sentModel}"
          | tokenModel: "${OpenNLPModels.tokenModel}"
          | nerModels: [
          |   "${OpenNLPModels.nerPersonModel}"
          |   ]
          | nerTypes: [
          |   "person"
          |   ]
          | srcFields: [
          |   "body"
          |   ]
          | idField: "docId"
          | separator: "|"
          |}
        """.stripMargin)

      val proc = new OpenNLPNerRecordProcessorFactory(config).getInstance()

      val record = Record(Seq(
        Cell("docId", "DOC-001"),
        Cell("category", "Sports"),
        Cell("title", "Mark mets Chris at Boston"),
        Cell("body", "Mr. Mark Warburton and Mr. Chris Heston are in Boston and Los Angeles."))
      )
      val result = proc.execute(Some(record))

      result.get.cellList.length must_==(2)

      result.get.cellList(0).name mustEqual "docId"
      result.get.cellList(0).value mustEqual "DOC-001"

      result.get.cellList(1).name mustEqual "body_person"
      result.get.cellList(1).value mustEqual "Mark Warburton|Chris Heston"
    }
  }

}
