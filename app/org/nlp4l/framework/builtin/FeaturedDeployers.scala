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

import java.io.File

import com.typesafe.config.Config
import dispatch.{Http, as, url}
import org.nlp4l.framework.processors.{Deployer, DeployerFactory}

import scala.concurrent.Await

import scala.concurrent.ExecutionContext.Implicits.global

class HttpFileTransferDeployerFactory(settings: Config) extends DeployerFactory(settings) {

  override def getInstance: Deployer = {
    new HttpFileTransferDeployer(
      getStrParamRequired("deployToUrl"),
      getStrParamRequired("deployToFile")
    )
  }
}

class HttpFileTransferDeployer(toUrl: String, toFile: String) extends Deployer {

  override def deploy(file: String): Unit = {
    val req = url(toUrl).addQueryParameter("file", toFile) <<< new File(file)
    val f = Http(req OK as.String)
    Await.result(f, scala.concurrent.duration.Duration.Inf)
  }
}
