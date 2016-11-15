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
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.mllib.classification.SVMWithSGD
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.regression.LabeledPoint
import play.api.Logger
import play.api.libs.json.Json



class RankingSVMTrainerFactory(settings: Config) extends TrainerFactory(settings: Config) {
  def getInstance(): Trainer = {
    new RankingSVMTrainer(getIntParam("numIterations", 2000))
  }
}

class RankingSVMTrainer(val numIterations: Int) extends PseudoPairwiseTrainer  {

  private val logger = Logger(this.getClass)

  def train(featureNames: Array[String],
            features: Vector[Vector[(Int, Vector[Float])]],
            progress: TrainingProgress) : String = {
    logger.info("train start.")
    logger.info("featureNames: " + featureNames.toSeq)
    logger.info("numIterations: " + numIterations)

    val dataSet: Vector[Vector[LabeledPoint]] = features.map(_.map { data => LabeledPoint(data._1.toDouble, Vectors.dense(data._2.map(_.toDouble).toArray)) })

    val trainingData: Vector[LabeledPoint] = dataSet.flatMap {
      data => {
        val combi2 = data.combinations(2)
        val pairwiseData = combi2.filterNot(a => a(0).label == a(1).label).map { a =>
          if (a(0).label > a(1).label) {
            val vector = Vectors.dense((a(0).features.toArray zip a(1).features.toArray).map { a => a._1 - a._2 })
            new LabeledPoint(1, vector)
          }
          else {
            val vector = Vectors.dense((a(0).features.toArray zip a(1).features.toArray).map { a => a._2 - a._1 })
            new LabeledPoint(1, vector)
          }
        }
        pairwiseData
      }
    }
    progress.report(10)
    if (logger.isDebugEnabled) {
      logger.debug("trainingData: size=" + trainingData.size + "\n" + trainingData.map( lp => (lp.label + " " + lp.features)).mkString("\n"))
    }

    val sc = SparkContextLocal.newSparkContext("RankingSVMTrainer")
    val model = try {
      SVMWithSGD.train(sc.makeRDD(trainingData), numIterations)
    }
    finally {
      if (sc != null)
        sc.stop()
    }
    val weights = model.weights.toArray
    val weightsJson = for (e <- featureNames.zipWithIndex) yield Json.obj("name" -> e._1, "weight" -> weights(e._2))
    val jsonModel = Json.obj(
      "name" -> "linearWeight",
      "type" -> "linearWeight",
      "weights" -> weightsJson
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

object SparkContextLocal {

  def newSparkContext(appName: String): SparkContext = {
    new SparkContext("local", appName, new SparkConf().set("spark.driver.allowMultipleContexts", "true"))
  }

}
