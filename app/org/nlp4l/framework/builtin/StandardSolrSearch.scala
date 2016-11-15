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

package org.nlp4l.framework.builtin

import java.net.URLEncoder

import org.nlp4l.framework.models.{CellAttribute, CellType}

class StandardSolrSearchCellAttribute(val searchOn: String, collection: String, idField: String, hlField: String, val separatedBy: String,
                                      name: String, cellType: CellType, isEditable: Boolean, isSortable: Boolean, userDefinedHashCode:(Any) => Int = null)
  extends CellAttribute(name, cellType, isEditable, isSortable, userDefinedHashCode) {
  override def format(cell: Any): String = {
    val url = URLEncoder.encode(s"$searchOn", "UTF-8")
    val queries = if(separatedBy != null){
      cell.toString.split(separatedBy).map{ a => a.trim }
    }
    else{
      Array(cell.toString)
    }

    val links = for(query <- queries) yield {
      val encodedQuery = URLEncoder.encode(s"$query", "UTF-8")
      hlField match {
        case null => s"""<a href="/searchResult/solr/$url/$collection/$encodedQuery?id=$idField">$query</a>"""
        case _ => s"""<a href="/searchResult/solr/$url/$collection/$encodedQuery?id=$idField&hl=$hlField">$query</a>"""
      }
    }

    val replaceMap = queries zip links
    val replaced = embedLinks(replaceMap, cell.toString)
    replaced
  }

  private def embedLinks(replaceMap: Seq[(String, String)], record: String): String = {
    def loop(str: String, res: String): String = {
      if (str.isEmpty) res
      else {
        // find all candidates to replace and sort by term length
        val candidates = replaceMap.filter{ case (q, l) => str.startsWith(q)}.sortBy(_._1.length)(Ordering.Int.reverse)
        if (candidates.nonEmpty) {
          // replace matched substring with the longest candidate
          val replace = candidates(0)
          loop(str.substring(replace._1.length), res + replace._2)
        } else {
          loop(str.substring(1), res + str(0))
        }
      }
    }

    loop(record, "")
  }
}
