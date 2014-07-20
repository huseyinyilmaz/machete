(function() {
    "use strict";

    var machete_url = "/u";

    $(function(){
        var $button = $("#shorten_button");
        var $input = $("#url_input");

        $button.click(function(){
            var value = $input.val();
            $.post(machete_url,
                   {url: value}).done(function(result){
                       console.log('done');
                   }).error(function(result){
                       console.log('error');
                   }).always(function(){
                       console.log('always');
                   });
            console.log (value);

        });
    });
}());
