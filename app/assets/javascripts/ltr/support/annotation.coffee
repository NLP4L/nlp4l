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
  qid = url.pop()
  path = url.pop()
  ltrid = url.pop()

  if(qid == "annotation")
    ltrid = path
  if(qid == "annotation")
    qid = "0"

  check_input = () ->
    searchText = $('#searchText').val()

    if (searchText.length > 0)
      $('#save').prop('disabled', false)
    else
      $('#save').prop('disabled', true)

  $('#searchText').keyup ->
    check_input()
