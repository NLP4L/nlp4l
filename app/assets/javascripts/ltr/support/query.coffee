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
  url.pop()
  ltrid = url.pop()

  $('#query').change ->
    files = $('#query')[0].files
    if (files.length == 1)
      $('#upload-button').prop('disabled', false)
    else
      $('#upload-button').prop('disabled', true)

  $('#upload').click ->
    action = '/ltr/query/' + ltrid
    $('#uploadform').attr('action', action)
    $('#uploadform').attr('method', 'POST')
    $('#uploadform').submit()

  $('#clear').click ->
    action = '/ltr/query/' + ltrid + '/clear'
    $('#uploadform').attr('action', action)
    $('#uploadform').attr('method', 'POST')
    $('#uploadform').submit()


  $('#clearEach').click ->
      elems = $('input[name="btSelectItem"]')
      clearIds = []
      for e in elems
        if e.checked
          id = e.value
          clearurl = '/ltr/query/' + ltrid + '/clear/' + id
          $.ajax
            url: clearurl,
            type: 'POST',
            async: false,
            success: (data) ->
              clearIds.push id
              ret = data
      $table.bootstrapTable('refresh');
      $('#clearEachModal').modal('hide');

  $('#deleteEach').click ->
    elems = $('input[name="btSelectItem"]')
    delIds = []
    for e in elems
      if e.checked
        id = e.value
        delurl = '/ltr/query/' + ltrid + '/delete/' + id
        $.ajax
          url: delurl,
          type: 'DELETE',
          async: false,
          success: (data) ->
            delIds.push id
    $table.bootstrapTable('refresh');
    $('#deleteEachModal').modal('hide');
