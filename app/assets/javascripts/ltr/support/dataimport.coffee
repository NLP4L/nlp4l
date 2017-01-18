###
 Copyright 2017 org.NLP4L

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
###

$ ->
  url = location.pathname.split('/')
  url.pop()
  ltrid = url.pop()

  $('#importFile').change ->
    files = $('#importFile')[0].files
    if (files.length == 1)
      $('#import').prop('disabled', false)
    else
      $('#import').prop('disabled', true)

  $('#import').click ->
    formdata = new FormData($('#importform').get(0));
    action = '/ltr/dataImport/' + ltrid + '/importData'
    $.ajax
      url: action,
      type: 'POST',
      data : formdata,
      cache       : false,
      contentType : false,
      processData : false,
      dataType    : "text",
      success: (data, textStatus, jqXHR) ->
        json = JSON.parse(data)
        if (json.status)
            $('#import_result_msg').html("<div class=\"alert alert-success\">" + json.msg + "</div>")
         else
            $('#import_result_msg').html("<div class=\"alert alert-danger\">" + json.msg + "</div>")
       error: (jqXHR, textStatus, errorThrown) ->
            $("#import_result_msg").html("<div class=\"alert alert-danger\">" + textStatus + ": " + errorThrown + "<br>" + jqXHR.responseText + "</div>")

