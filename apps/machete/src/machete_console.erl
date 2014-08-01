%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% functions that will be called from console commands
%%% @end
%%% Created : 27 Jul 2014 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(machete_console).

%% API
-export([create_schema/1,
         backup/1,
         restore/1,
         txt_backup/1,
         restore_from_txt_backup/1,
         connect/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes an empty database
%% @end
%%--------------------------------------------------------------------

create_schema([]) ->
    lager:info("An empty mnesia db is being initialized."),
    machete_mnesia:create_schema().

%%--------------------------------------------------------------------
%% @doc
%% Backups mnensia database to given file name
%% @end
%%--------------------------------------------------------------------
-spec backup([File_name::list()]| []) -> ok.
backup([]) -> backup(["machete_mnesia_backup.dub"]);
backup([File_name]) ->
    lager:info("Mnasia db backup is being started. file_name=~p",
               [File_name]),
    machete_mnesia:backup(File_name),
    lager:info("Mnesia db backup is completed. file_name=~p",
              [File_name]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Restores mnensia database from given file name
%% @end
%%--------------------------------------------------------------------

-spec restore([File_name::list()]|[]) -> ok.
restore([]) -> restore(["machete_mnesia_backup.dub"]);
restore([File_name]) ->
    lager:info("Mnasia db restore is being started. file_name=~p",
               [File_name]),
    machete_mnesia:create_from_backup(File_name),
    lager:info("Mnesia db restore is completed. file_name=~p",
              [File_name]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Backups mnensia database to given file name in text format
%% @end
%%--------------------------------------------------------------------
-spec txt_backup([File_name::list()]|[]) -> ok.
txt_backup([]) -> txt_backup(["machete_mnesia_backup.txt"]);
txt_backup([File_name]) ->
    lager:info("Mnasia db backup is being started. file_name=~p",
               [File_name]),
    machete_mnesia:txt_backup(File_name),
    lager:info("Mnesia db backup is completed. file_name=~p",
              [File_name]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Restores mnensia database from given file name in text format
%% @end
%%--------------------------------------------------------------------
-spec restore_from_txt_backup([File_name::list()]|[]) -> ok.
restore_from_txt_backup([]) -> restore_from_txt_backup(["machete_mnesia_backup.txt"]);
restore_from_txt_backup([File_name]) ->
    lager:info("Mnasia db restore is being started. file_name=~p",
               [File_name]),
    machete_mnesia:create_from_txt_backup(File_name),
    lager:info("Mnesia db restore is completed. file_name=~p",
              [File_name]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Connects current node to mnesia cluster
%% @end
%%--------------------------------------------------------------------
connect([Node_name]) when is_list(Node_name) ->
    lager:info("Mnesia db is being connect to a new cluster ~p",
               [Node_name]),
    Node = list_to_atom(Node_name),
    pong = net_adm:ping(Node),
    machete_mnesia:connect(Node).





%%%===================================================================
%%% Internal functions
%%%===================================================================
