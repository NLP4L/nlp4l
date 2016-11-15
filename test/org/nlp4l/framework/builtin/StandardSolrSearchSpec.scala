package org.nlp4l.framework.builtin

import java.net.URLEncoder

import org.nlp4l.framework.models.CellType
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

class StandardSolrSearchSpec extends Specification with Mockito {

  val searchOn = "http://localhost:8983/solr"
  def cellAtt(separatedBy: String, hlField: String = null) = new StandardSolrSearchCellAttribute(searchOn, "collection1", "id", hlField, separatedBy, "cell1", CellType.StringType, true, false)

  "StandardSolrSearchCellAttribute" should {
    "format cell value with a separator" in {
      val mockCell = mock[Any]
      mockCell.toString() returns "マック,マクド,マクドナルド"

      val expected =
        List(
          s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マック", "UTF-8")}?id=id">マック</a>""",
          s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マクド", "UTF-8")}?id=id">マクド</a>""",
          s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マクドナルド", "UTF-8")}?id=id">マクドナルド</a>"""
        ).mkString(",")
      cellAtt(",").format(mockCell) mustEqual expected
    }

    "format cell value with multiple separators" in {
      val mockCell = mock[Any]
      mockCell.toString() returns "マック,マクド => マクドナルド"

      val expected =
        List(
          s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マック", "UTF-8")}?id=id">マック</a>""",
          s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マクド", "UTF-8")}?id=id">マクド</a>"""
        ).mkString(",") +
          " => " + s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マクドナルド", "UTF-8")}?id=id">マクドナルド</a>"""
      cellAtt("(,)|(=>)").format(mockCell) mustEqual expected
    }

    "return empty string when cell value is empty" in {
      val mockCell = mock[Any]
      mockCell.toString() returns ""
      cellAtt(",").format(mockCell) mustEqual ""
    }

    "format cell value with highlight" in {
      val mockCell = mock[Any]
      mockCell.toString() returns "マック,マクド,マクドナルド"

      val expected =
        List(
          s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マック", "UTF-8")}?id=id&hl=body">マック</a>""",
          s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マクド", "UTF-8")}?id=id&hl=body">マクド</a>""",
          s"""<a href="/searchResult/solr/${URLEncoder.encode(searchOn, "UTF-8")}/collection1/${URLEncoder.encode("マクドナルド", "UTF-8")}?id=id&hl=body">マクドナルド</a>"""
        ).mkString(",")
      cellAtt(",", "body").format(mockCell) mustEqual expected
    }

  }
}
