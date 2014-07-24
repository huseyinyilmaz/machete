(function() {
    "use strict";

    var machete_url = "/u";

    $(function(){
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
                alert(url);
            }).error(function(result){
                       console.log('error',result);
                   }).always(function(){
                       console.log('always');
                   });
            console.log (value);

        });
    });
}());
