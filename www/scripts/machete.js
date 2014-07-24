(function() {
    "use strict";

    var machete_url = "/u";

    $(function(){
        var $result_span = $("#result_div");
        var result_template_text = $("#result_template").html();
        var $button = $("#shorten_button");
        var $input = $("#url_input");
        $button.click(function(){
            var value = $input.val();
            $.post(machete_url, value).done(function(result){
                // load json
                result = JSON.parse(result);
                console.log('done', result);
                var uri = result.uri;
                window.result = result;
                var url = window.location.protocol + '//' + window.location.host + uri;

                $result_span.html(Mustache.render(result_template_text,
                                                  {'url': url}));
            }).error(function(result){
                       console.log('error',result);
                   }).always(function(){
                       console.log('always');
                   });
            console.log (value);

        });
    });
}());
