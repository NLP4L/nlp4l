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
  ltrid = url.pop()
  currentLtrid = url.pop()
  currentLtrid = url.pop()

  if(currentLtrid == "ltrdashboard")
    currentLtrUrl = "/0"
  else
    currentLtrUrl = "/" + currentLtrid
    
  ltrurl = ""
  if(ltrid != "new")
    ltrurl = "/" + ltrid
    $('#load-button').prop('disabled', false)
    $('#save-button').prop('disabled', false)
    $('#delete-button').prop('disabled', false)
  else
    $('#load-button').prop('disabled', true)
    $('#save-button').prop('disabled', true)
    $('#delete-button').prop('disabled', true)

  if(currentLtrid == ltrid)
  　　$('#delete-button').prop('disabled', true)

  
  check_input = () ->
    name = $('#name').val()
    annotationType = $('#annotationType').val()
    labelMax = $('#labelMax').val()
    trainerFactoryClassName = $('#trainerFactoryClassName').val()
    deployerFactoryClassName = $('#deployerFactoryClassName').val()
    searchUrl = $('#searchUrl').val()
    featureExtractUrl = $('#featureExtractUrl').val()
    featureExtractConfig = $('#featureExtractConfig').val()
    docUniqField = $('#docUniqField').val()
    docTitleField = $('#docTitleField').val()
    docBodyField = $('#docBodyField').val()

    if (name.length > 0 && annotationType.length > 0 && labelMax.length > 0 && trainerFactoryClassName.length > 0 && deployerFactoryClassName.length > 0 && searchUrl.length > 0 && featureExtractUrl.length > 0 && featureExtractConfig.length > 0 && docUniqField.length > 0 && docTitleField.length > 0&& docBodyField.length > 0)
      $('#save-button').prop('disabled', false)
    else
      $('#save-button').prop('disabled', true)

  $('#name').keyup ->
    name = $('#name').val()
    check_input()
  
  $('#annotationType').keyup ->
    check_input()
    
  $('#labelMax').keyup ->
    check_input()
    
  $('#trainerFactoryClassName').keyup ->
    check_input()
    
  $('#deployerFactoryClassName').keyup ->
    check_input()

  $('#searchUrl').keyup ->
    check_input()
    
  $('#featureExtractUrl').keyup ->
    check_input()
    
  $('#featureProgressUrl').keyup ->
    check_input()
    
  $('#featureRetrieveUrl').keyup ->
    check_input()
    
  $('#docUniqField').keyup ->
    check_input()

  $('#save-button').click ->
    name = $('#name').val()
    annotationType = $('#annotationType').val()
    labelMax = $('#labelMax').val()
    trainerFactoryClassName = $('#trainerFactoryClassName').val()
    trainerFactoryClassSettings = $('#trainerFactoryClassSettings').val()
    deployerFactoryClassName = $('#deployerFactoryClassName').val()
    deployerFactoryClassSettings = $('#deployerFactoryClassSettings').val()
    searchUrl = $('#searchUrl').val()
    featureExtractUrl = $('#featureExtractUrl').val()
    featureExtractConfig = $('#featureExtractConfig').val()
    docUniqField = $('#docUniqField').val()
    docTitleField = $('#docTitleField').val()
    docBodyField = $('#docBodyField').val()
    $.ajax
      url: '/ltr/config' + ltrurl,
      type: 'POST',
      contentType: 'text/json',
      data: JSON.stringify({
      	"name": name, 
      	"annotationType":annotationType,
      	"labelMax":labelMax,
      	"trainerFactoryClassName":trainerFactoryClassName,
      	"trainerFactoryClassSettings":trainerFactoryClassSettings,
      	"deployerFactoryClassName":deployerFactoryClassName,
      	"deployerFactoryClassSettings":deployerFactoryClassSettings,
      	"searchUrl":searchUrl,
      	"featureExtractUrl":featureExtractUrl,
      	"featureExtractConfig":featureExtractConfig,
      	"docUniqField":docUniqField,
      	"docTitleField":docTitleField,
      	"docBodyField":docBodyField

      }),
      success: (data, textStatus, jqXHR) ->
        jump = '/ltrdashboard'+currentLtrUrl+'/config/' + data.ltrid
        location.replace(jump)


  $('#load-button').click ->
    jump = '/ltrdashboard/'+ltrid+'/config/'+ltrid
    location.replace(jump)
    
  $('#delete-ltrconfig').click ->
    $.ajax
      url: '/ltr/config' + ltrurl,
      type: 'DELETE',
      success: (data, textStatus, jqXHR) ->
        jump = '/ltrdashboard'+currentLtrUrl+'/config'
        if(currentLtrid == ltrid)
          jump = '/ltrdashboard/0/config'
          
        location.replace(jump)

    