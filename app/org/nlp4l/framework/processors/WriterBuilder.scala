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

import com.typesafe.config.{ Config, ConfigFactory }
import org.nlp4l.framework.dao.JobDAO
import play.api.Logger

import scala.collection.convert.WrapAsScala._
import scala.concurrent.Await


object WriterBuilder {

  val logger = Logger(this.getClass)

  def build(jobDAO: JobDAO, jobId: Int): Writer = {
    val job = Await.result(jobDAO.get(jobId), scala.concurrent.duration.Duration.Inf)
    build(job.config)
  }

  def build(cfg: String): Writer = {
    val config = ConfigFactory.parseString(cfg)

    val pConf = config.getConfig("writer")
    try {
      val className = pConf.getString("class")
      val constructor = Class.forName(className).getConstructor(classOf[Config])

      val facP = constructor.newInstance(settings(config)).asInstanceOf[WriterFactory]
      facP.getInstance()
    } catch {
      case e: Exception => {
        logger.error(e.getMessage)
        throw e
      }
    }
  }

  def settings(config: Config): Config = {
    val gSettings = getConfig(config, "settings")
    if(config.hasPath("writer")){
      val wConf = config.getConfig("writer")
      val lSettings = getConfig(wConf, "settings")
      lSettings.withFallback(gSettings)
    } else {
      gSettings
    }
  }

  def getConfig(src: Config, key: String): Config = {
    if(src.hasPath(key)) src.getConfig(key) else ConfigFactory.empty()
  }
}
