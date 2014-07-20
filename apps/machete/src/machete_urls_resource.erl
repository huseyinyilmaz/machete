-module(machete_urls_resource).
-export([init/1,
         to_html/2,
         allowed_methods/2,
         process_post/2,
         post_is_create/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    lager:debug("XXXXXXXXXXXXXXXXXXXXXXXX"),

    {{trace, "/tmp"}, undefined}.
    %% {ok, undefined}.

allowed_methods(ReqData, Ctx) ->
    {['POST'], ReqData, Ctx}.


post_is_create(Request, Ctx) ->
    {false, Request, Ctx}.


process_post(ReqData, Ctx) ->
    Body = mochijson2:encode([{<<"uri">>, <<"/u/1234">>}]),
    lager:debug("Response Body = ~p", [Body]),
    ReqData2 = wrq:set_resp_body(Body, ReqData),
    {true, ReqData2, Ctx}.
