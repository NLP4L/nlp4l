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

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import com.typesafe.config.Config
import dispatch.{Http, as, url}
import play.api.Logger
import resource._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global


class HttpFileTransferDeployerFactory(settings: Config) extends DeployerFactory(settings: Config) {
  def getInstance(): Deployer = {
    new HttpFileTransferDeployer(getStrParamRequired("deployToUrl"), getStrParamRequired("deployToFile"))
  }
}

class HttpFileTransferDeployer(val toUrl: String, val toFile: String) extends Deployer  {

  private val logger = Logger(this.getClass)

  def deploy(model_data: String) : Unit = {
    logger.info("deploy start. url=" + toUrl + ", file=" + toFile)
    val file = File.createTempFile("nlp4l-", ".json")
    for (output <- managed(new PrintWriter(new BufferedWriter(new FileWriter(file, false))))) {
      output.print(model_data)
    }
    val req = url(toUrl).addQueryParameter("file", toFile) <<< file
    val f = Http(req OK as.String)
    Await.result(f, scala.concurrent.duration.Duration.Inf)
    logger.info("deploy end.")
  }
}
