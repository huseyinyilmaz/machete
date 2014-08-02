%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Module that holds code for initialization of mnesia database
%%% @end
%%% Created : 13 Jul 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(machete_mnesia).

%% API
-export([create_schema/0,
         create_from_backup/1,
         backup/1,
         create_from_txt_backup/1,
         txt_backup/1,
         check_schema/0,
         add_client/1,
         connect/1]).


-include_lib("db.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes an empty schema
%% @end
%%--------------------------------------------------------------------
create_schema()->
    ensure_mnesia_dir(),
    init_schema(),
    create_mnesia_tables(absent_tables()).

%%--------------------------------------------------------------------
%% @doc
%% Initialize an empty schema and load data from backup file
%% @end
%%--------------------------------------------------------------------
create_from_backup(File_name)->
    ensure_mnesia_dir(),
    init_schema(),
    create_mnesia_tables(absent_tables()),
    {atomic, _} = mnesia:restore(File_name, [{default_op, recreate_tables}]).

%%--------------------------------------------------------------------
%% @doc
%% Backup current database
%% @end
%%--------------------------------------------------------------------
backup(File_name) ->
    ok = mnesia:backup(File_name).

%%--------------------------------------------------------------------
%% @doc
%% Initialize an empty schema and load data from text backup file
%% @end
%%--------------------------------------------------------------------
create_from_txt_backup(File_name)->
    ensure_mnesia_dir(),
    init_schema(),
    create_mnesia_tables(absent_tables()),
    mnesia:load_textfile(File_name).

%%--------------------------------------------------------------------
%% @doc
%% Backup current mnesia db to text file
%% @end
%%--------------------------------------------------------------------
txt_backup(File_name)->
    mnesia:dump_to_textfile(File_name).

%%--------------------------------------------------------------------
%% @doc
%% Check if schema has tables.
%% @end
%%--------------------------------------------------------------------
check_schema() ->
    case absent_tables() of
        [] -> true;
        _ -> false
    end.


%% remove_client(Node)->
%%     %% R1 = [{Name, mnesia:del_table_copy(Name, Node)} || {Name, _} <- types()],
%%     R1 = 1,
%%     R2 = rpc:call(Node, mnesia, stop,[]),
%%     R3 = rpc:call(Node, mnesia, delete_schema, [[Node]]),
%%     %% Remove node from cluster
%%     R4 = mnesia:del_table_copy(schema, Node),
%%     lager:debug("R1=~p R2=~p R3=~p R4=~p", [R1, R2, R3, R4]).


add_client(Node) ->
    R1 = mnesia:change_config(extra_db_nodes, [Node]),
    % change schema table type so client can also store disc_copies.
    R2 = mnesia:change_table_copy_type(schema, Node, disc_copies),
    R3 = [{Name, mnesia:add_table_copy(Name, Node, Type)} || {Name, Type} <- types()],
    lager:debug("add client results R1=~p, R2=~p, R3=~p", [R1, R2, R3]).

%% It turns out that mnesia nodes are not removable
%% disconnect(Node) ->
%%     Result = rpc:call(Node, ?MODULE, remove_client, [node()]),
%%     lager:debug("rpc call result = ~p", [Result]).

connect(Node) ->
    %% Add current node to claster as db node.
    %% This will get schema from master to current node.
    Result = rpc:call(Node, ?MODULE, add_client, [node()]),
    lager:debug("rpc call result = ~p", [Result]).



%%%===================================================================
%%% Internal functions
%%%===================================================================

start()->
    lager:debug("starting mnesia."),
    ok = mnesia:start().


stop()->
    lager:debug("stop mnesia."),
    mnesia:stop().

%%% creates an empty
init_schema() ->
    stop(),
    ensure_mnesia_not_running(),
    lager:debug("creating mnesia schema"),
    _ = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    lager:debug("starting mnesia"),
    start(),
    ensure_mnesia_running().


create_mnesia_tables(Tables) ->
    lists:foreach(fun ({Tab, TabDef}) ->
                          case mnesia:create_table(Tab, TabDef) of
                              {atomic, ok} -> ok;
                              {aborted, Reason} ->
                                  throw({error, {table_creation_failed,
                                                 Tab, TabDef, Reason}})
                          end
                  end, Tables),
    ok = mnesia:wait_for_tables(names(Tables), 30000).



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

-spec dir() -> list().
dir() -> mnesia:system_info(directory).

-spec table_list() -> table_name().
table_list()->
    lists:sort(lists:delete(schema, mnesia:system_info(tables))).

-spec absent_tables() -> [table_definition()].
absent_tables()->
    Table_list = table_list(),
    lists:foldl(fun({Table_name, _}=Item, Sum)->
                        case lists:member(Table_name, Table_list) of
                            true ->
                                Sum;
                            false ->
                                [Item | Sum]
                        end
                end,
                [],
                definitions()).


wait_for(Condition) ->
    lager:info("Waiting for ~p...", [Condition]),
    timer:sleep(1000).

-spec names(Tables::[table_definition()]) -> table_name().
names(Tables) -> [Name || {Name, _} <- Tables].

-spec get_type(proplists:property()) -> table_types().
get_type(Attrs)->
    get_type_by_name_list([Attr || {Attr, _} <- Attrs]).

-spec get_type_by_name_list(AttrNames::[atom()]) -> table_types().
get_type_by_name_list(AttrNames) ->
    Types = [disc_only_copies, disc_copies, ram_copies],
    case lists:filter(fun(E) -> lists:member(E, Types) end,
                      AttrNames) of
        [] -> error("No type found");
        [Type] -> Type;
        _ -> error("Multiple types found")
    end.

-spec types()-> [table_definition()].
types()->
    [{Name, get_type(Properties)} || {Name, Properties} <- definitions()].

-spec definitions() -> [].
definitions()->
    [{url,
      [{record_name, url},
       {attributes, record_info(fields, url)},
       {disc_only_copies, [node()]}]},
     {counter,
      [{record_name, counter},
       {attributes, record_info(fields, counter)},
       {disc_copies, [node()]}]}
    ].
