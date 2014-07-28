'use strict';

var $ = require('jquery');
var Mustache = require('mustache');


var machete_url = "/u";
var $result_span = $("#result_div");
var result_template_text = $("#result_template").html();
var $input = $("#url_input");


$('#shorten_form').submit(function(e) {
    e.preventDefault();
    $.post(machete_url, $input.val()).done(function(result) {
        var loc = window.location,
            url = loc.protocol + '//' + loc.host + result.uri;
        $result_span.html(Mustache.render(result_template_text, {'url': url}));
    }).error(function(result) {
        console.log('error', result);
    });
});
