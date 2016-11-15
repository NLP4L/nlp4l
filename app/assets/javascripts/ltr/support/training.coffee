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

  url = location.pathname.split('/')
  mid = url.pop()
  url.pop()
  ltrid = url.pop()

  $('#create').click ->
    elems = $('input[name="checkFeature"]')
    fids = ""
    for e in elems
      if e.checked
        fid = e.value
        if (fids != "")
            fids += ","
        fids += fid
    $.ajax
      url: '/ltr/training/' + ltrid + '?features=' + fids,
      type: 'GET',
      success: (data) ->
        jump = '/ltrdashboard/' + ltrid + '/training/' + data.mid
        location.replace(jump)
    return

  $('#delete').click ->
    $.ajax
        url:  '/ltr/training/' + ltrid + '/delete/' + mid,
        type: 'DELETE',
        async: false,
        success: (data) ->
            jump = '/ltrdashboard/' + ltrid + '/training'
            location.replace(jump)
    return




