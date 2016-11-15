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

package org.nlp4l.framework.builtin.spark.mllib

import com.typesafe.config.Config
import org.apache.spark.mllib.classification.{LogisticRegressionWithLBFGS, NaiveBayes}
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.tree.{DecisionTree, RandomForest}
import org.apache.spark.mllib.util.MLUtils._
import org.apache.spark.rdd.RDD
import org.nlp4l.framework.models.{Record, _}
import org.nlp4l.framework.processors._
import play.api.Logger

import scala.util.Try
import scalax.file.Path

class TrainAndModelProcessorFactory(settings: Config) extends ProcessorFactory(settings) {

  override def getInstance: Processor = {
    new TrainAndModelProcessor(
      getStrParamRequired("modelDir"),
      getStrParam("algorithm", AlgorithmSupport.Default),
      getConfigParam("algorithmParams", null),
      getDoubleParam("trainTestRate", 0.7)
    )
  }
}

class TrainAndModelProcessor(val modelDir: String,
                             val algorithm: String,
                             val algorithmParams: Config,
                             val trainTestRate: Double
                            )
  extends Processor
    with CommonProcessor {

  val settings = new ModelDirSettings(modelDir)

  private val logger = Logger(this.getClass)

  override def execute(dataDict: Option[Dictionary]): Option[Dictionary] = {

    val path = Path.fromString(settings.MODEL_FILE_DIR)
    Try(path.deleteRecursively(continueOnFailure = false))

    val labelNameMap: Map[String, Int] = readLabelFile(settings.LABEL_FILE, settings.LABEL_FILE_SEP)

    val sc = SparkContextLocal.newSparkContext("TrainAndModelProcessor")
    val (precision) = try {
      // Load training data in LIBSVM format.
      val data = loadLibSVMFile(sc, settings.DATA_FILE)
      // Split data into training and test
      val splits = data.randomSplit(Array(trainTestRate, 1 - trainTestRate), seed = 11L)
      val training = splits(0).cache()
      val test: RDD[LabeledPoint] = splits(1)

      // Run training algorithm to build the model
      val model: ModelSupport = algorithm match {
        case AlgorithmSupport.NaiveBayes => {
          val lambda = getAlgorithmParamAsDouble("lambda", 1.0)
          val modelType = getAlgorithmParam("modelType", "multinomial")
          logger.info("algorithm: " + AlgorithmSupport.NaiveBayes + ", lambda: " + lambda + ", modelType: " + modelType)
          new NaiveBayesModelSupport(
            NaiveBayes.train(training, lambda, modelType)
          )
        }
        case AlgorithmSupport.LogisticRegressionWithLBFGS => {
          val numClasses = labelNameMap.keySet.size
          logger.info("algorithm: " + AlgorithmSupport.LogisticRegressionWithLBFGS + ", numClasses: " + numClasses)
          new LogisticRegressionModelSupport(
            new LogisticRegressionWithLBFGS()
            .setNumClasses(numClasses)
            .run(training)
          )
        }
        case AlgorithmSupport.DecisionTree => {
          val numClasses = labelNameMap.keySet.size
          val categoricalFeaturesInfo = Map[Int, Int]()
          val impurity = getAlgorithmParam("impurity", "gini") //  "gini" (recommended)
          val maxDepth = getAlgorithmParamAsInt("maxDepth", 5) // (suggested value: 5)
          val maxBins = getAlgorithmParamAsInt("maxBins", 32) // (suggested value: 32)
          logger.info("algorithm: " + AlgorithmSupport.DecisionTree + ", numClasses: " + numClasses +
            ", impurity: " + impurity + ", maxDepth: " + maxDepth + ", maxBins: " + maxBins)
          new DecisionTreeModelSupport(
            DecisionTree.trainClassifier(training, numClasses, categoricalFeaturesInfo,
              impurity, maxDepth, maxBins)
          )
        }
        case AlgorithmSupport.RandomForest => {
          val numClasses = labelNameMap.keySet.size
          val categoricalFeaturesInfo = Map[Int, Int]()
          val numTrees = getAlgorithmParamAsInt("numTrees", 10) // Number of trees in the random forest.
          val featureSubsetStrategy = getAlgorithmParam("featureSubsetStrategy", "auto") // Let the algorithm choose.
          val impurity =  getAlgorithmParam("impurity", "gini") //  "gini" (recommended)
          val maxDepth =  getAlgorithmParamAsInt("maxDepth", 4) // ((suggested value: 4)
          val maxBins = getAlgorithmParamAsInt("maxBins", 100) // (suggested value: 100)
          logger.info("algorithm: " + AlgorithmSupport.RandomForest + ", numClasses: " + numClasses +
            ", numTrees: " + numTrees + ", featureSubsetStrategy: " + featureSubsetStrategy +
            ", impurity: " + impurity + ", maxDepth: " + maxDepth + ", maxBins: " + maxBins)
          new RandomForestModelSupport(
            RandomForest.trainClassifier(training, numClasses, categoricalFeaturesInfo,
              numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)
          )
        }
        case _ => throw new IllegalArgumentException("unknown algorithm: " + algorithm)
      }

      // Save model
      model.save(sc, settings.MODEL_FILE_DIR)

      // Compute raw scores on the test set.
      val predictionAndLabels = test.map {
        case LabeledPoint(label, features) => {
          val prediction = model.predict(features)
          (prediction, label)
        }
      }

      // Get evaluation metrics.
      val metrics = new MulticlassMetrics(predictionAndLabels)
      val precision: Double = metrics.precision
      (precision)
    }
    finally {
      if (sc != null)
        sc.stop()
    }

    val resultMsg = s"success: precision = ${precision}"
    val dict = Dictionary(Seq(
      Record(Seq(
        Cell("result", resultMsg)
      ))))
    Some(dict)
  }

  def getAlgorithmParam(name: String, default: String): String = {
    if (algorithmParams != null && algorithmParams.hasPath(name)) algorithmParams.getString(name) else default
  }
  def getAlgorithmParamAsDouble(name: String, default: Double): Double = {
    if (algorithmParams != null && algorithmParams.hasPath(name)) algorithmParams.getDouble(name) else default
  }
  def getAlgorithmParamAsInt(name: String, default: Int): Int = {
    if (algorithmParams != null && algorithmParams.hasPath(name)) algorithmParams.getInt(name) else default
  }
}
