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

import java.io._

object FileUtil {

  def makeTempTextFile(text: Seq[String], encoding: String, prefix: String = "nlp4l-"): File = {
    val file = File.createTempFile(prefix, ".txt")
    val fos = new FileOutputStream(file)
    val osw = new OutputStreamWriter(fos, encoding)
    val bw = new BufferedWriter(osw)
    val pw = new PrintWriter(bw)
    try{
      text.foreach(pw.println(_))
    }
    finally{
      pw.close()
      bw.close()
      osw.close()
      fos.close()
    }
    file
  }

  def delete(file: File): Unit = {
    file.delete()
  }
}
