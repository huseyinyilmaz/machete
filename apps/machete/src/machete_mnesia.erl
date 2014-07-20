%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 13 Jul 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(machete_mnesia).

%% API
-export([create/0,
         init/0]).

-include("machete.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates mnesia tables
%% @end
%%--------------------------------------------------------------------

init() ->
    mnesia:start(),
    ensure_mnesia_running(),
    check_mnesia_tables(),
    ensure_mnesia_dir().

create() ->
    lists:foreach(fun ({Tab, TabDef}) ->
                          TabDef1 = proplists:delete(match, TabDef),
                          case mnesia:create_table(Tab, TabDef1) of
                              {atomic, ok} -> ok;
                              {aborted, Reason} ->
                                  throw({error, {table_creation_failed,
                                                 Tab, TabDef1, Reason}})
                          end
                  end, definitions()),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Ensures that mnesia is running
%% @end
%%--------------------------------------------------------------------
ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        starting ->
            wait_for(mnesia_running),
            ensure_mnesia_running();
        Reason when Reason =:= no; Reason =:= stopping ->
            throw({error, mnesia_not_running})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_mnesia_tables()->
    Tables = table_list(),
    lager:info('table_list=~p', [Tables]).

ensure_mnesia_not_running() ->
    case mnesia:system_info(is_running) of
        no ->
            ok;
        stopping ->
            wait_for(mnesia_not_running),
            ensure_mnesia_not_running();
        Reason when Reason =:= yes; Reason =:= starting ->
            throw({error, mnesia_unexpectedly_running})
    end.

ensure_mnesia_dir() ->
    MnesiaDir = dir() ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok ->
            ok
    end.

dir() -> mnesia:system_info(directory).


absent_tables()->
    Table_list = table_list,
    lists:foldl(fun({table_name, table_def}, Sum)->ok end,
                [],
                definitions)

table_list()->
    lists:sort(lists:delete(schema, mnesia:system_info(tables))).

wait_for(Condition) ->
    lager:info("Waiting for ~p...", [Condition]),
    timer:sleep(1000).

definitions()->
    [{machete_url,
      [{record_name, url},
       {attributes, record_info(fields, url)},
       {disc_copies, [node()]}]}].
