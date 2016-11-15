###
 Copyright 2015 org.NLP4L

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
  $table = $('#table')

  url = location.pathname.split('/')

  runId = url.pop()

  jobId = url.pop()
  
  $('#delete-result').click ->
    $.ajax
      url: '/job/result/' + jobId + '/' + runId,
      type: 'DELETE',
      success: (data, textStatus, jqXHR) ->
        jump = '/dashboard/job/' + jobId
        location.replace(jump)

  $('#delete-record').click ->
    elems = $('input[name="btSelectItem"]')
    delIds = []
    for e in elems
      if e.checked 
        id = e.value
        delurl = '/job/result/record/' + jobId+ '/' + runId + '/' + id
        $.ajax
          url: delurl,
          type: 'DELETE',
          success: (data) ->
            delIds.push id
    jump = '/dashboard/job/result/' + jobId+ '/' + runId
    location.replace(jump)

  $('#addrecord-button').click ->
    form = $('#addrecord-form')
    d = form.serialize()
    $.ajax
      url: '/job/result/record/add/' + jobId + '/' + runId,
      type: 'POST',
      data: d
      success: (data, textStatus, jqXHR) ->
        jump = '/dashboard/job/result/' + jobId+ '/' + runId
        location.replace(jump)
  
  $('#editrecord-button').click ->
    form = $('#editrecord-form')
    recordId = $('#editrecord-form input[name="id"]').val()
    d = form.serialize()
    $.ajax
      url: '/job/result/record/update/' + jobId + '/' + runId + '/' + recordId,
      type: 'POST',
      data: d
      success: (data, textStatus, jqXHR) ->
        jump = '/dashboard/job/result/' + jobId+ '/' + runId
        location.replace(jump)

  $('#export-button').click ->
    action = '/job/result/export/' + jobId + '/' + runId
    $('#exportform').attr('action', action)
    $('#exportform').attr('method', 'GET')
    $('#exportform').submit()


