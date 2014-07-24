-module(machete_url_resource).
-export([init/1,
         to_html/2,
         resource_exists/2,
         previously_existed/2,
         moved_permanently/2
        ]).

-record(context, {url}).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.


init([]) ->
    lager:debug("XXXXXXXXXXXXXXXXXXXXXXXX"),
    {{trace, "/tmp"}, #context{}}.
    %% {ok, undefined}.

resource_exists(ReqData, State) ->
    {false, ReqData, State}.

previously_existed(ReqData, State) ->
    Code = wrq:path_info(code, ReqData),
    lager:debug("url=~p", [Code]),
    case machete_mnesia:get_url(Code) of
        not_found ->
            lager:debug("Code ~p not found", [Code]),
            {false, ReqData, State};
        Url ->
            lager:debug("Code ~p found = ~p", [Code, Url]),
            {true, ReqData, State#context{url=Url}}
    end.

moved_permanently(ReqData, #context{url=Url}=State) ->
    lager:debug("Redirect url ~p", [Url]),
    {{true, binary_to_list(Url)}, ReqData, State}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    to_json(ReqData, State).
                                

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    Code = wrq:path_info(code, ReqData),
    lager:debug("url=~p", [Code]),
    Url = machete_mnesia:get_url(Code),
    lager:debug("url=~p", [Url]),
    {<<"XXXXX">>, ReqData, State}.
