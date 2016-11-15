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

package org.nlp4l.framework.processors

import com.typesafe.config.Config
import org.nlp4l.framework.models._
import org.specs2.mutable.Specification

class ConfigSpec extends Specification {

  val conf1 =
    """
      |{
      |  settings : {
      |    greeting : "this is global"
      |    name : "Mike"
      |    code : "global-1"
      |  }
      |  dictionary : [
      |    {
      |      class : org.nlp4l.framework.processors.TestDictionaryAttributeFactory
      |      settings : {
      |        code : "dic-1"
      |      }
      |    }
      |  ]
      |
      |  processors : [
      |    {
      |      class : org.nlp4l.framework.processors.TestProcessorFactory
      |      settings : {
      |        greeting : "this is processor"
      |        code : "proc-1"
      |      }
      |    }
      |    {
      |      class : org.nlp4l.framework.processors.Test2ProcessorFactory
      |      settings : {
      |        code : "proc-2"
      |      }
      |    }
      |  ]
      |
      |  validators : [
      |    {
      |      class : org.nlp4l.framework.processors.TestValidatorFactory
      |      settings : {
      |        name : "Tom"
      |        code : "valid-1"
      |      }
      |    }
      |    {
      |      class : org.nlp4l.framework.processors.Test2ValidatorFactory
      |      settings : {
      |        greeting : "this is validator"
      |        code : "valid-2"
      |      }
      |    }
      |  ]
      |
      |  writer :
      |    {
      |      class : org.nlp4l.framework.processors.TestWriterFactory
      |      settings : {
      |        code : "write-1"
      |      }
      |    }
      |
      |  deployer :
      |    {
      |      class : org.nlp4l.framework.processors.TestDeployerFactory
      |      settings : {
      |        code : "deploy-1"
      |      }
      |    }
      |}
    """.stripMargin

  "global settings" should {

    "can be seen from DictionaryAttributeFactory that doesn't override them" in {
      val dicAttr = new ProcessorChainBuilder().dicBuild(conf1)
      dicAttr.cellAttributeList(0).name must_== "this is global"
      dicAttr.cellAttributeList(1).name must_== "Mike"
    }

    "can be seen from ProcessorFactories that don't override them" in {
      val pChain = new ProcessorChainBuilder().procBuild(1, conf1).result()
      val tp0 : TestProcessor = pChain.chain(0).asInstanceOf[TestProcessor]
      tp0.settings.getString("name") must_== "Mike"
      val tp1 : Test2Processor = pChain.chain(1).asInstanceOf[Test2Processor]
      tp1.settings.getString("greeting") must_== "this is global"
      tp1.settings.getString("name") must_== "Mike"
    }

    "can be seen from ValidatorFactories that don't override them" in {
      val vChain = new ValidatorChainBuilder().build(conf1).result()
      val vp0 : TestValidator = vChain.chain(0).asInstanceOf[TestValidator]
      vp0.settings.getString("greeting") must_== "this is global"
      val vp1 : Test2Validator = vChain.chain(1).asInstanceOf[Test2Validator]
      vp1.settings.getString("name") must_== "Mike"
    }

    "can be seen from WriterFactory that doesn't override them" in {
      val dp0 : TestWriter = WriterBuilder.build(conf1).asInstanceOf[TestWriter]
      dp0.settings.getString("greeting") must_== "this is global"
      dp0.settings.getString("name") must_== "Mike"
    }

    "can be overridden by DictionaryAttributeFactory" in {
      val dicAttr = new ProcessorChainBuilder().dicBuild(conf1)
      dicAttr.cellAttributeList(2).name must_== "dic-1"
    }

    "can be overridden by ProcessorFactories" in {
      val pChain = new ProcessorChainBuilder().procBuild(1, conf1).result()
      val tp0 : TestProcessor = pChain.chain(0).asInstanceOf[TestProcessor]
      tp0.settings.getString("greeting") must_== "this is processor"
      tp0.settings.getString("code") must_== "proc-1"
      val tp1 : Test2Processor = pChain.chain(1).asInstanceOf[Test2Processor]
      tp1.settings.getString("code") must_== "proc-2"
    }

    "can be seen from ValidatorFactories that don't override them" in {
      val vChain = new ValidatorChainBuilder().build(conf1).result()
      val vp0 : TestValidator = vChain.chain(0).asInstanceOf[TestValidator]
      vp0.settings.getString("name") must_== "Tom"
      vp0.settings.getString("code") must_== "valid-1"
      val vp1 : Test2Validator = vChain.chain(1).asInstanceOf[Test2Validator]
      vp1.settings.getString("greeting") must_== "this is validator"
      vp1.settings.getString("code") must_== "valid-2"
    }

    "can be overridden by WriterFactories" in {
      val dp0 : TestWriter = WriterBuilder.build(conf1).asInstanceOf[TestWriter]
      dp0.settings.getString("code") must_== "write-1"
    }

    "can be overridden by DeployerFactories" in {
      val dp0 : TestDeployer = DeployerBuilder.build(conf1).asInstanceOf[TestDeployer]
      dp0.settings.getString("code") must_== "deploy-1"
    }
  }

  val conf2_missing_dic =
    """
      |{
      |  settings : {
      |    greeting : "this is global"
      |    name : "Mike"
      |    code : "global-1"
      |  }
      |
      |  processors : [
      |    {
      |      class : org.nlp4l.framework.processors.TestProcessorFactory
      |      settings : {
      |        greeting : "this is processor"
      |        code : "proc-1"
      |      }
      |    }
      |    {
      |      class : org.nlp4l.framework.processors.Test2ProcessorFactory
      |      settings : {
      |        code : "proc-2"
      |      }
      |    }
      |  ]
      |
      |  validators : [
      |    {
      |      class : org.nlp4l.framework.processors.TestValidatorFactory
      |      settings : {
      |        name : "Tom"
      |        code : "valid-1"
      |      }
      |    }
      |    {
      |      class : org.nlp4l.framework.processors.Test2ValidatorFactory
      |      settings : {
      |        greeting : "this is validator"
      |        code : "valid-2"
      |      }
      |    }
      |  ]
      |
      |  writer :
      |    {
      |      class : org.nlp4l.framework.processors.TestWriterFactory
      |      settings : {
      |        code : "write-1"
      |      }
      |    }
      |}
    """.stripMargin

  val conf3_missing_procs =
    """
      |{
      |  settings : {
      |    greeting : "this is global"
      |    name : "Mike"
      |    code : "global-1"
      |  }
      |  dictionary : [
      |    {
      |      class : org.nlp4l.framework.processors.TestDictionaryAttributeFactory
      |      settings : {
      |        code : "dic-1"
      |      }
      |    }
      |  ]
      |
      |  validators : [
      |    {
      |      class : org.nlp4l.framework.processors.TestValidatorFactory
      |      settings : {
      |        name : "Tom"
      |        code : "valid-1"
      |      }
      |    }
      |    {
      |      class : org.nlp4l.framework.processors.Test2ValidatorFactory
      |      settings : {
      |        greeting : "this is validator"
      |        code : "valid-2"
      |      }
      |    }
      |  ]
      |
      |  writer :
      |    {
      |      class : org.nlp4l.framework.processors.TestWriterFactory
      |      settings : {
      |        code : "write-1"
      |      }
      |    }
      |}
    """.stripMargin

  val conf4_missing_dicclass =
    """
      |{
      |  dictionary : [
      |    {
      |      settings : {
      |        code : "dic-1"
      |      }
      |    }
      |  ]
      |
      |  processors : [
      |    {
      |      class : org.nlp4l.framework.processors.TestProcessorFactory
      |      settings : {
      |        greeting : "this is processor"
      |        code : "proc-1"
      |      }
      |    }
      |  ]
      |}
    """.stripMargin

  val conf5_missing_procclass =
    """
      |{
      |  dictionary : [
      |    {
      |      class : org.nlp4l.framework.processors.TestDictionaryAttributeFactory
      |      settings : {
      |        code : "dic-1"
      |      }
      |    }
      |  ]
      |
      |  processors : [
      |    {
      |      settings : {
      |        greeting : "this is processor"
      |        code : "proc-1"
      |      }
      |    }
      |  ]
      |}
    """.stripMargin

  val conf6_missing_validclass =
    """
      |{
      |  dictionary : [
      |    {
      |      class : org.nlp4l.framework.processors.TestDictionaryAttributeFactory
      |      settings : {
      |        code : "dic-1"
      |      }
      |    }
      |  ]
      |
      |  processors : [
      |    {
      |      class : org.nlp4l.framework.processors.TestProcessorFactory
      |      settings : {
      |        greeting : "this is processor"
      |        code : "proc-1"
      |      }
      |    }
      |  ]
      |
      |  validators : [
      |    {
      |      settings : {
      |        name : "Tom"
      |        code : "valid-1"
      |      }
      |    }
      |  ]
      |}
    """.stripMargin

  val conf7_missing_writeclass =
    """
      |{
      |  dictionary : [
      |    {
      |      class : org.nlp4l.framework.processors.TestDictionaryAttributeFactory
      |      settings : {
      |        code : "dic-1"
      |      }
      |    }
      |  ]
      |
      |  processors : [
      |    {
      |      class : org.nlp4l.framework.processors.TestProcessorFactory
      |      settings : {
      |        greeting : "this is processor"
      |        code : "proc-1"
      |      }
      |    }
      |  ]
      |
      |  writer :
      |    {
      |      settings : {
      |        code : "write-1"
      |      }
      |    }
      |}
    """.stripMargin

  "ProcessorChain.validateConf" should {
    "return true if the Config file has all members" in {
      ProcessorChain.validateConf(conf1) must_== true
    }

    "return false if the config doesn't have required entries" in {
      ProcessorChain.validateConf(conf2_missing_dic) must_== false
      ProcessorChain.validateConf(conf3_missing_procs) must_== false
    }

    "return false if the config doesn't have class entry" in {
      ProcessorChain.validateConf(conf4_missing_dicclass) must_== false
      ProcessorChain.validateConf(conf5_missing_procclass) must_== false
      ProcessorChain.validateConf(conf6_missing_validclass) must_== false
      ProcessorChain.validateConf(conf7_missing_writeclass) must_== false
    }
  }
}

class TestDictionaryAttributeFactory(settings: Config) extends DictionaryAttributeFactory(settings){
  override def getInstance(): DictionaryAttribute = {
    new DictionaryAttribute("empty", Seq(
      CellAttribute(getStrParam("greeting", "foo"), CellType.StringType, false, false),
      CellAttribute(getStrParam("name", "bar"), CellType.StringType, false, false),
      CellAttribute(getStrParam("code", "baz"), CellType.StringType, false, false))
    )
  }
}

class TestProcessorFactory(settings: Config) extends ProcessorFactory(settings){
  override def getInstance(): Processor = {
    new TestProcessor(settings)
  }
}

class TestProcessor(val settings: Config) extends Processor {
}

class Test2ProcessorFactory(settings: Config) extends ProcessorFactory(settings){
  override def getInstance(): Processor = {
    new Test2Processor(settings)
  }
}

class Test2Processor(val settings: Config) extends Processor {
}

class TestValidatorFactory(settings: Config) extends ValidatorFactory(settings){
  override def getInstance(): Validator = {
    new TestValidator(settings)
  }
}

class TestValidator(val settings: Config) extends Validator {
  def validate (data: Option[Dictionary]): Tuple2[Boolean, Seq[String]] = {
    (true, Seq())
  }
}

class Test2ValidatorFactory(settings: Config) extends ValidatorFactory(settings){
  override def getInstance(): Validator = {
    new Test2Validator(settings)
  }
}

class Test2Validator(val settings: Config) extends Validator {
  def validate (data: Option[Dictionary]): Tuple2[Boolean, Seq[String]] = {
    (true, Seq())
  }
}

class TestWriterFactory(settings: Config) extends WriterFactory(settings){
  override def getInstance(): Writer = {
    new TestWriter(settings)
  }
}

class TestWriter(val settings: Config) extends Writer {
  override def write (data: Option[Dictionary], dictionaryAttribute: DictionaryAttribute): String = {
    ""
  }
}

class TestDeployerFactory(settings: Config) extends DeployerFactory(settings){
  override def getInstance(): Deployer = {
    new TestDeployer(settings)
  }
}

class TestDeployer(val settings: Config) extends Deployer {
  override def deploy (filename: String): Unit = {
  }
}
