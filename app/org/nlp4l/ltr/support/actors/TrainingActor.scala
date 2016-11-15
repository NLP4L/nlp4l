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

package org.nlp4l.ltr.support.actors

import org.nlp4l.ltr.support.procs.TrainingExecutor
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import play.api.Logger


trait TrainingProgressReport {
  private val logger = Logger(this.getClass)
  
  def report(ltrid: Int, runid: Int, to: ActorRef, progressValue: Int): Unit = {
    logger.info("TrainingSetMsg received: " + ltrid + "#" + runid + " [" + progressValue + "]")
    to ! TrainingSetProgressMsg(ltrid, runid, progressValue)
  }
}

class TrainingActor extends Actor {
  private val logger = Logger(this.getClass)
  
  override def receive: Receive = {
    case TrainingStartMsg(trainingRequest: TrainingRequest) => {
      val trainingExecutor = new TrainingExecutor(sender)
      logger.info("TrainingStartMsg received: " + trainingRequest.ltrid + "#" + trainingRequest.runid)
      trainingExecutor.execute(trainingRequest)
    }
  }
}


