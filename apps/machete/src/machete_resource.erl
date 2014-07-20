-module(machete_resource).
-export([
    init/1,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    lager:debug("XXXXXXXXXXXXXXXXXXXXXXXX"),

    {{trace, "/tmp"}, undefined}.
    %% {ok, undefined}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    Data = wrq:path_info(code, ReqData),
    {Data, ReqData, State}.

