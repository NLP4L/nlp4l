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

import com.typesafe.config.Config
import play.api.Logger
import play.api.libs.json.{Json}




class PRankTrainerFactory(settings: Config) extends TrainerFactory(settings: Config) {
  def getInstance(): Trainer = {
    new PRankTrainer(getIntParam("numIterations", 2000))
  }
}

class PRankTrainer(val numIterations: Int) extends PointwiseTrainer  {

  private val logger = Logger(this.getClass)

  def train(featureNames: Array[String],
            features: Array[Vector[Float]],
            labels: Array[Int],
            maxLabel: Int,
            progress: TrainingProgress) : String = {
    logger.info("train start.")
    logger.info("featureNames: " + featureNames.toSeq)
    logger.info("maxLabel: " + maxLabel)
    logger.info("labels length: " + labels.length)
    logger.info("numIterations: " + numIterations)
    val prank = new PRank(features, labels, featureNames.length, maxLabel, numIterations, progress)
    val wb = prank.train()
    val weights = for(e <- featureNames.zipWithIndex) yield Json.obj("name" -> e._1, "weight" -> wb._1(e._2))
    val bs = Json.toJson(wb._2)
    val jsonModel = Json.obj(
        "name" -> "prank",
        "type" -> "prank",
        "weights" -> weights,
        "bs" -> bs
    )
    val jsonObj = Json.obj(
      "model" -> jsonModel
    )
    val jsonText = Json.prettyPrint(jsonObj)
    logger.info("train end." + "\n" + jsonText)
    progress.report(100)
    jsonText
  }
}

class PRank(x: Array[Vector[Float]], y: Array[Int], featureNum: Int, maxLabel: Int, numIterations: Int, progress: TrainingProgress) {

  var w: Vector[Float] = Vector.fill(featureNum)(0F)
  val b: Array[Float] = Array.fill(maxLabel)(0F)
  b.update(maxLabel - 1, Float.MaxValue)
  val yt = Array.fill(maxLabel - 1)(0)
  val tau = Array.fill(maxLabel - 1)(0)
  val r = scala.util.Random

  def train(): (Vector[Float], Vector[Float]) = {
    for(t <- Range(1, numIterations)){
      val t0 = r.nextInt(x.size)
      val predictY = predict(0, weight(x(t0)))
      if(predictY != y(t0)){
        for(r <- Range(1, b.size)){
          if(y(t0) <= r) yt(r - 1) = -1
          else yt(r - 1) = 1
          val f = weight(x(t0))
          if((f - b(r - 1)) * yt(r - 1) <= 0) tau(r - 1) = yt(r - 1)
          else tau(r - 1)
        }
        val tt = tau.foldLeft(0)((a, b) => a + b)
        w = for((a, b) <- w zip x(t0)) yield { a + tt * b }
        for(r <- Range(1, b.size)){
          b(r - 1) = b(r - 1) - tau(r - 1)
        }
      }
      if (t % (numIterations / 100) == 0) {
        progress.report(t / (numIterations / 100))
      }
    }
    (w, b.take(b.size-1).toVector)
  }

  def predict(r: Int, f: Float): Int = {
    if(f - b(r) < 0) r + 1
    else predict(r + 1, f)
  }

  def weight(x: Vector[Float]): Float = {
    require(w.size == x.size)
    (for((a, b) <- w zip x) yield a * b) sum
  }

  def dumpResult(): Unit = {
    println(s"w=($w), b=(${b(0)}, ${b(1)}, ${b(2)}})")
  }

  def test(x: Vector[Float]): Unit = {
    println(s"$x -> rank of ${predict(0, weight(x))}")
  }
}
