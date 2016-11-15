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

import java.io._

import org.apache.spark.mllib.classification.{LogisticRegressionModel, NaiveBayesModel}
import org.apache.spark.mllib.linalg.Vector
import org.apache.spark.mllib.tree.model.{DecisionTreeModel, RandomForestModel}
import org.apache.spark.{SparkConf, SparkContext}
import org.nlp4l.lucene.stats.WordCounts
import org.nlp4l.lucene.{IReader, RawReader}
import resource._

trait CommonProcessor {

  def selectFeatures(reader: RawReader, field: String, minDF: Int = 1, maxDFPercent: Double = 1.0, maxNumTerms: Int = -1): Set[String] = {
    val docFreqs = WordCounts.countDF(reader, field, Set.empty[String])
    val numDocs = reader.numDocs.toDouble
    val wordsFilteredByDF = docFreqs.filter(_._2 >= minDF).filter(_._2 / numDocs <= maxDFPercent).map(_._1).toSet
    if (maxNumTerms > 0)
      reader.topTermsByDocFreq(field, maxNumTerms).map(_._1).toSet & wordsFilteredByDF
    else
      wordsFilteredByDF
  }

  def selectLabelMap(reader: RawReader, labelField: String): Map[String, Int] = {
    val labelMap = reader.field(labelField) match {
      case Some(fieldInfo) => fieldInfo.terms.map(_.text).zipWithIndex.toMap
      case _ => Map.empty[String, Int]
    }
    labelMap
  }

  def selectFieldValues(reader: RawReader, docIds: List[Int], fields: Seq[String]): List[Map[String, Seq[String]]] = {
    docIds.map(id => reader.document(id) match {
      case Some(doc) => {
        fields.map(f => (f, doc.getValue(f).getOrElse(List.empty))).toMap
      }
      case _ => Map.empty[String, Seq[String]]
    })
  }

  def readLabelFile(labelFile: String, labelFileSep: String): Map[String, Int] = {
    val builder = Map.newBuilder[String, Int]
    for (input <- managed(new BufferedReader(new FileReader(new File(labelFile))))) {
      def read(): Unit = input.readLine() match {
        case null => ()
        case line => {
          val cols = line.split(labelFileSep)
          builder += (cols(0) -> cols(1).toInt)
          read()
        }
      }
      read()
    }
    builder.result()
  }

  def writeLabelFile(labelMap:  Map[String, Int], labelFile: String, labelFileSep: String): Unit = {
    for (output <- managed(new PrintWriter(labelFile))) {
      labelMap.toList sortBy(x => x._2) foreach { case (value, label) =>
        output.println("%s%s%d".format(value, labelFileSep, label))
      }
    }
  }

  def readFeaturesFile(featureFile: String): Set[String] = {
    val file = new File(featureFile)
    // build word set from feature file
    val builder = Set.newBuilder[String]
    if (file.exists()) {
      for (input <- managed(new BufferedReader(new FileReader(file)))) {
        def read(): Unit = input.readLine() match {
          case null => ()
          case line => {
            builder += (line.trim)
            read()
          }
        }
        read()
      }
    }
    builder.result()
  }

  def readTermBoostsFile(boostFile: String): Map[String, Double] = {
    val file = new File(boostFile)
    // build term boosts map
    val builder = Map.newBuilder[String, Double]
    if (file.exists()) {
      for (input <- managed(new BufferedReader(new FileReader(file)))) {
        def read(): Unit = input.readLine() match {
          case null => ()
          case line => {
            val cols = line.split(",")
            builder += (cols(0).trim -> cols(1).trim.toDouble)
            read()
          }
        }
        read()
      }
    }
    builder.result()
  }

  def writeDataFile(labels: Seq[Int], vectors: Stream[Seq[AnyVal]], out: String, outputSep: String, idValues: Seq[String]): Unit = {
    val file: File = new File(out)
    for(output <- managed(new BufferedWriter(new FileWriter(file)))) {
      if (idValues.isEmpty) {
        labels.zip(vectors).foreach { case (label: Int, vector: Seq[AnyVal]) => {
          // output label
          output.write(label.toString + outputSep)
          // output index:value pairs for LIBSVM format. indices are one-based and in ascending order.
          val vecWithIdx = vector.zipWithIndex.filter(_._1 != 0).map(t => (t._2 + 1).toString + ":" + t._1.toString)
          output.write(vecWithIdx.mkString(" "))
          output.newLine()
        }}
      } else {
        (idValues, labels, vectors).zipped.foreach {case (id, label, vector) => {
          // output idField value
          output.write(id + outputSep)
          // output label
          output.write(label.toString + outputSep)
          // output index:value pairs for LIBSVM format. indices are one-based and in ascending order.
          val vecWithIdx = vector.zipWithIndex.filter(_._1 != 0).map(t => (t._2 + 1).toString + ":" + t._1.toString)
          output.write(vecWithIdx.mkString(" "))
          output.newLine()
        }}
      }
      output.flush()
    }
  }

  def writeWordsFile(reader: IReader, features: Seq[String], textField: String,  wordsFileOut: String, wordsFileSep: String): Unit = {
    val dfMap = WordCounts.countDF(reader, textField, features.toSet)
    for (output <- managed(new PrintWriter(new FileWriter(wordsFileOut)))) {
      features.zipWithIndex.foreach{case(word, id) => output.println((id + 1).toString + wordsFileSep + word + wordsFileSep + dfMap(word))}
    }
  }

  def readWordsFile(wordsFile: String, wordsFileSep: String): Map[String, (Int, Long)] = {
    val builder = Map.newBuilder[String, (Int, Long)]
    for (input <- managed(new BufferedReader(new FileReader(new File(wordsFile))))) {
      def read(): Unit = input.readLine() match {
        case null => ()
        case line => {
          val cols = line.split(wordsFileSep)
          builder += (cols(1) -> (cols(0).toInt, cols(2).toLong))
          read()
        }
      }
      read()
    }
    builder.result()
  }

  def writeTFIDFParamsFile(reader: IReader, textField: String, tfMode: String, smthTerm: Double, idfMode: String, termBoostsFile: String, tfidfFileOut: String, tfidfFileSep: String): Unit = {
    val numDocs = reader.numDocs
    val maxTF = if (tfMode == "m") reader.topTermsByTotalTermFreq(textField, 1)(0)._3 else -1
    for (output <- managed(new PrintWriter(new FileWriter(tfidfFileOut)))) {
      output.println("numDocs" + tfidfFileSep + numDocs)
      output.println("maxTF" + tfidfFileSep + maxTF)
      output.println("tfMode" + tfidfFileSep + tfMode)
      output.println("smthTerm" + tfidfFileSep + smthTerm)
      output.println("idfMode" + tfidfFileSep + idfMode)
      output.println("termBoostsFile" + tfidfFileSep + (if (termBoostsFile != null) termBoostsFile else "" ))
    }
  }

  def readTFIDFParamsFile(tfidfParamFile: String, tfidfFileSep: String): (Int, Long, String, Double, String, String) = {
    val builder = Map.newBuilder[String, String]
    for (input <- managed(new BufferedReader(new FileReader(new File(tfidfParamFile))))) {
      def read(): Unit = input.readLine() match {
        case null => ()
        case line => {
          val cols = line.split(tfidfFileSep)
          if (cols.length > 1)
            builder += (cols(0) -> cols(1))
          else
            builder += (cols(0) -> null)
          read()
        }
      }
      read()
    }
    val map = builder.result()
    (map("numDocs").toInt, map("maxTF").toLong, map("tfMode"), map("smthTerm").toDouble, map("idfMode"), map("termBoostsFile"))
  }
}

object SparkContextLocal {

  def newSparkContext(appName: String): SparkContext = {
    new SparkContext("local", appName, new SparkConf().set("spark.driver.allowMultipleContexts", "true"))
  }

}

class ModelDirSettings(modelDir: String) {

  val LUCENE_INDEX_DIR = new File(modelDir, "lucene").getAbsolutePath
  val LABEL_FILE = new File(modelDir, "label.txt").getAbsolutePath
  val DATA_FILE  = new File(modelDir, "data.txt").getAbsolutePath
  val WORDS_FILE = new File(modelDir, "words.txt").getAbsolutePath
  val TFIDF_FILE = new File(modelDir, "tfidf-params.txt").getAbsolutePath
  val VALUES_FILE_DIR = new File(modelDir, "values").getAbsolutePath
  val MODEL_FILE_DIR = new File(modelDir, "model").getAbsolutePath

  val LABEL_FILE_SEP  = "\t"
  val DATA_FILE_SEP  = " "
  val WORDS_FILE_SEP  = "\t"
  val TFIDF_FILE_SEP  = "\t"

}

object AlgorithmSupport {
  val NaiveBayes = "NaiveBayes"
  val LogisticRegressionWithLBFGS = "LogisticRegressionWithLBFGS"
  val DecisionTree = "DecisionTree"
  val RandomForest = "RandomForest"

  val Default = NaiveBayes
}

abstract class ModelSupport() extends Serializable  {
  def predict(testData: Vector): Double
  def save(sc: SparkContext, path: String): Unit
}

class NaiveBayesModelSupport(model: NaiveBayesModel) extends ModelSupport {
  override def predict(features: Vector): Double = {
    model.predict(features)
  }
  override def save(sc: SparkContext, path: String): Unit = {
    model.save(sc, path)
  }
}
object NaiveBayesModelSupport {
  def load(sc: SparkContext, path: String): NaiveBayesModelSupport = {
    new NaiveBayesModelSupport(NaiveBayesModel.load(sc, path))
  }
}

class LogisticRegressionModelSupport(model: LogisticRegressionModel) extends ModelSupport {
  override def predict(features: Vector): Double = {
    model.predict(features)
  }
  override def save(sc: SparkContext, path: String): Unit = {
    model.save(sc, path)
  }
}
object LogisticRegressionModelSupport {
  def load(sc: SparkContext, path: String): LogisticRegressionModelSupport = {
    new LogisticRegressionModelSupport(LogisticRegressionModel.load(sc, path))
  }
}

class DecisionTreeModelSupport(model: DecisionTreeModel) extends ModelSupport {
  override def predict(features: Vector): Double = {
    model.predict(features)
  }
  override def save(sc: SparkContext, path: String): Unit = {
    model.save(sc, path)
  }
}
object DecisionTreeModelSupport {
  def load(sc: SparkContext, path: String): DecisionTreeModelSupport = {
    new DecisionTreeModelSupport(DecisionTreeModel.load(sc, path))
  }
}

class RandomForestModelSupport(model: RandomForestModel) extends ModelSupport {
  override def predict(features: Vector): Double = {
    model.predict(features)
  }
  override def save(sc: SparkContext, path: String): Unit = {
    model.save(sc, path)
  }
}
object RandomForestModelSupport {
  def load(sc: SparkContext, path: String): RandomForestModelSupport = {
    new RandomForestModelSupport(RandomForestModel.load(sc, path))
  }
}



