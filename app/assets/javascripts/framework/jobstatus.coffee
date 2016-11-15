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

  getHeight = ->
    $(window).height() - $('h1').outerHeight(true)

  $ ->
    $table.bootstrapTable
      height: getHeight()
      columns: [ [
        {
          title: 'Status'
          field: 'status'
          align: 'center'
          valign: 'middle'
        }
        {
          title: 'Name'
          field: 'name'
          align: 'center'
          valign: 'middle'
        }
        {
          title: 'Progress'
          field: 'progress'
          align: 'center'
          valign: 'middle'
        }
      ] ]