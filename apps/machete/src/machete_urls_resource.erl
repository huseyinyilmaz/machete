-module(machete_urls_resource).
-export([init/1,
         allowed_methods/2,
         process_post/2,
         post_is_create/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, Ctx) ->
    {['POST'], ReqData, Ctx}.


post_is_create(Request, Ctx) ->
    {false, Request, Ctx}.


process_post(ReqData, Ctx) ->
    Url = machete_utils:normalize_url(wrq:req_body(ReqData)),
    Code = machete_db:insert_url(Url),
    Response = mochijson2:encode([{<<"uri">>, << <<"/u/">>/binary, Code/binary>>}]),
    lager:debug("Response Data = ~p", [Response]),
    ReqData2 = wrq:set_resp_header("Content-Type", "application/json",
                                   wrq:set_resp_body(Response, ReqData)),
    {true, ReqData2, Ctx}.
