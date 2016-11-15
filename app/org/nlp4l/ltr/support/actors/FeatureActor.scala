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

import org.nlp4l.ltr.support.models.FeatureExtractDTOs
import org.nlp4l.ltr.support.procs.FeatureExtractor

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.actorRef2Scala
import play.api.Logger


trait FeatureProgressReport {
  private val logger = Logger(this.getClass)
  
  def report(ltrid: Int, to: ActorRef, progressValue: Int, message: String): Unit = {
    logger.info("FeatureSetMsg received: " + ltrid + " [" + progressValue + "]")
    to ! FeatureExtractSetProgressMsg(ltrid, progressValue, message)
  }

}

class FeatureActor extends Actor {
  private val logger = Logger(this.getClass)
  
  override def receive: Receive = {
    case FeatureExtractStartMsg(dtos: FeatureExtractDTOs) => {
      val featureExtraction = new FeatureExtractor(sender)
      logger.info("FeatureStartMsg received: " + dtos.ltrid)
      featureExtraction.execute(dtos)
    }
  }
}


