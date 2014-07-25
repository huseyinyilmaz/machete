-module(machete_config).

-export([
    dispatch/0,
    web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
        {["u"], machete_urls_resource, []},
        {["u", code], machete_url_resource, []},
        {["assets", '*'], machete_static_resource, [{root, "www"}]}
    ]).

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "log"},
        {dispatch, dispatch()}
    ].
