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

import java.net.{URLEncoder, URLDecoder}

import org.apache.solr.client.solrj.impl._
import org.apache.solr.client.solrj.request.QueryRequest
import org.apache.solr.common.SolrDocument
import org.apache.solr.common.params.ModifiableSolrParams
import play.api.libs.json.Json
import play.api.mvc.Action
import play.api.mvc.Controller
import collection.JavaConversions._

class SearchResult extends Controller {

  def searchResultSolr(url: String, collection: String, encodedQuery: String) = Action { request =>
    {
      val encodedUrl = URLEncoder.encode(url, "UTF-8")
      val query = URLDecoder.decode(encodedQuery, "UTF-8")
      val idField = request.getQueryString("id").getOrElse("id")
      val hlField = request.getQueryString("hl")
      Ok(org.nlp4l.framework.views.html.solrSearchResult(encodedUrl, collection, encodedQuery, query, idField, hlField))
    }
  }

  def searchBySolr(encodedUrl: String, collection: String, encodedQuery: String) = Action { request =>
    {
      val offset = request.getQueryString("offset") match {
        case Some(x) if x != "" => x.toInt
        case _ => 0
      }
      val limit = request.getQueryString("limit") match {
        case Some(x) => x.toInt
        case _ => 10
      }
      val url = URLDecoder.decode(encodedUrl, "UTF-8")
      val solr = new HttpSolrClient(url)
      val query = URLDecoder.decode(encodedQuery, "UTF-8")
      val params = new ModifiableSolrParams().add("q", query).set("start", offset).set("rows", limit)
      val idField = request.getQueryString("id").getOrElse("id")
      val hlField = request.getQueryString("hl")
      if(hlField != None){
        params.add("hl", "on").add("hl.fl", hlField.get).add("hl.usePhraseHighlighter", "true").
          add("hl.simple.pre", """<em class="lead">""").add("hl.simple.post", "</em>")
      }
      val req = new QueryRequest(params)
      val res = solr.query(collection, params)
      val hlRes = if(hlField != None) res.getHighlighting else null
      val docs = res.getResults
      val result = docs.map{ doc: SolrDocument =>
        val id = doc.getFieldValue(idField).toString
        if(hlRes == null) Map(idField -> id)
        else{
          val snippets = if(hlRes.get(id) != null){
            if(hlRes.get(id).get(hlField.get) != null){
              hlRes.get(id).get(hlField.get).mkString("...")
            }
            else "(No highlighting values)"
          }
          else "(No highlighting values)"
          Map(idField -> id, hlField.get -> snippets)
        }
      }
      val jsonResponse = Json.obj(
        "total" -> docs.getNumFound,
        "rows" -> Json.toJson(result.toList)
      )
      Ok(jsonResponse)
    }
  }
}
