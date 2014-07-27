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
-export([backup/1,
         restore/1,
         backup_to_txt/1,
         restore_from_txt/1]).

%%%===================================================================
%%% API
%%%===================================================================

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
    mnesia:backup(File_name),
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
    mnesia:restore(File_name, [{default_op, recreate_tables}]),
    lager:info("Mnesia db restore is completed. file_name=~p",
              [File_name]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Backups mnensia database to given file name in text format
%% @end
%%--------------------------------------------------------------------
-spec backup_to_txt([File_name::list()]|[]) -> ok.
backup_to_txt([]) -> backup(["machete_mnesia_backup.txt"]);
backup_to_txt([File_name]) ->
    lager:info("Mnasia db backup is being started. file_name=~p",
               [File_name]),
    mnesia:dump_to_textfile(File_name),
    lager:info("Mnesia db backup is completed. file_name=~p",
              [File_name]),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Restores mnensia database from given file name in text format
%% @end
%%--------------------------------------------------------------------
-spec restore_from_txt([File_name::list()]|[]) -> ok.
restore_from_txt([]) -> backup(["machete_mnesia_backup.txt"]);
restore_from_txt([File_name]) ->
    lager:info("Mnasia db restore is being started. file_name=~p",
               [File_name]),
    mnesia:load_textfile(File_name),
    lager:info("Mnesia db restore is completed. file_name=~p",
              [File_name]),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
