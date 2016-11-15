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

package org.nlp4l.framework.controllers

import dispatch._, Defaults._
import org.specs2.mutable.{BeforeAfter, Specification}

class JobControllerSpec extends Specification {

  trait context extends BeforeAfter {

    var server: TinyHttpServerThread = null

    def before = {
      server = new TinyHttpServerThread
      server.start()
      // TODO: remove sleep
      Thread.sleep(100)   // wait for TinyHttpServer to be started
    }

    def after = {
      val req = url("http://localhost:8983/shutdown")
      Http(req OK as.String)
      // TODO: remove sleep
      Thread.sleep(100)   // wait for TinyHttpServer to be down
    }
  }

  // this is needed as TinyHttpServer supports only one request at a time!
  args(sequential=true)

  "JobController.transferHttp" should {

    "consider EUC_JP encoding" in new context {
      val ENCODING = "EUC_JP"
      val hello = "こんにちは、世界！"
      JobController.transferHttp("http://localhost:8983/foo", "/tmp/bar", Seq(hello), ENCODING)
      val res = server.getRequest()
      new String(res._6, ENCODING).trim must_== hello
    }

    "consider UTF-8 encoding" in new context {
      val ENCODING = "UTF-8"
      val hello = "こんにちは、世界！"
      JobController.transferHttp("http://localhost:8983/foo", "/tmp/bar", Seq(hello), ENCODING)
      val res = server.getRequest()
      new String(res._6, ENCODING).trim must_== hello
    }
  }
}
