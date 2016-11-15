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

package org.nlp4l.framework.processors

import com.typesafe.config.Config
import org.nlp4l.framework.models.Dictionary
import org.nlp4l.framework.models.DictionaryAttribute
import org.nlp4l.framework.models.Record
import scala.collection.convert.WrapAsScala._


abstract class DictionaryAttributeFactory(settings: Config) extends ConfiguredFactory(settings){
  def getInstance(): DictionaryAttribute
}

abstract class ConfiguredFactory(val settings: Config){
  def getStrParam(name: String, default: String): String = {
    if(settings.hasPath(name)) settings.getString(name) else default
  }
  def getStrParamRequired(name: String): String = {
    // TODO: need to check this throws an Exception if there isn't the entry
    settings.getString(name)
  }
  def getIntParam(name: String, default: Int): Int = {
    if(settings.hasPath(name)) settings.getInt(name) else default
  }
  def getIntParamRequired(name: String): Int = {
    // TODO: need to check this throws an Exception if there isn't the entry
    settings.getInt(name)
  }
  def getLongParam(name: String, default: Long): Long = {
    if(settings.hasPath(name)) settings.getLong(name) else default
  }
  def getLongParamRequired(name: String): Long = {
    // TODO: need to check this throws an Exception if there isn't the entry
    settings.getLong(name)
  }
  def getDoubleParam(name: String, default: Double): Double = {
    if(settings.hasPath(name)) settings.getDouble(name) else default
  }
  def getDoubleParamRequired(name: String): Double = {
    // TODO: need to check this throws an Exception if there isn't the entry
    settings.getDouble(name)
  }
  def getBoolParam(name: String, default: Boolean): Boolean = {
    if(settings.hasPath(name)) settings.getBoolean(name) else default
  }
  def getBoolParamRequired(name: String): Boolean = {
    // TODO: need to check this throws an Exception if there isn't the entry
    settings.getBoolean(name)
  }
  def getStrListParam(name: String, default: Seq[String]): Seq[String] = {
    if(settings.hasPath(name)) settings.getStringList(name) else default
  }
  def getStrListParamRequired(name: String): Seq[String] = {
    // TODO: need to check this throws an Exception if there isn't the entry
    settings.getStringList(name)
  }
  def getConfigParam(name: String, default: Config): Config = {
    if(settings.hasPath(name)) settings.getConfig(name) else default
  }
  def getConfigParamRequired(name: String): Config = {
    settings.getConfig(name)
  }
  def getConfigListParam(name: String, default: Seq[Config]): Seq[Config] = {
    if(settings.hasPath(name)) settings.getConfigList(name) else default
  }
  def getConfigListParamRequired(name: String): Seq[Config] = {
    settings.getConfigList(name)
  }

}

abstract class ProcessorFactory(settings: Config) extends ConfiguredFactory(settings){
  def getInstance(): Processor
}

abstract class RecordProcessorFactory(settings: Config) extends ConfiguredFactory(settings){
  def getInstance(): RecordProcessor
}

trait Processor {
  def execute(data: Option[Dictionary]) : Option[Dictionary] = {
    data
  }
}

trait RecordProcessor {
  def execute(data: Option[Record]): Option[Record] = {
    data
  }
}


/**
 * Validater
 */
abstract class ValidatorFactory(settings: Config) extends ConfiguredFactory(settings){
  def getInstance(): Validator
}
trait Validator {
  def validate (data: Option[Dictionary]): Tuple2[Boolean, Seq[String]]
}


/**
 * Writer
 */
abstract class WriterFactory(settings: Config) extends ConfiguredFactory(settings){
  def getInstance(): Writer
}
trait Writer {
  def write (data: Option[Dictionary], dictionaryAttribute: DictionaryAttribute): String
}

/**
  * Deployer
  */
abstract class DeployerFactory(settings: Config) extends ConfiguredFactory(settings){
  def getInstance(): Deployer
}
trait Deployer {
  def deploy (file: String): Unit
}
