%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 29 Jul 2014 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(machete_db).

%% API
-export([insert_url/1,
         get_url/1]).

-include_lib("db.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

-spec insert_url(binary() | list()) -> binary().
insert_url(Url) when is_list(Url) -> insert_url(list_to_binary(Url));
insert_url(Url) ->
    Code = get_url_code(),
    mnesia:dirty_write(#url{code=Code, url=Url}),
    Code.
    %% mnesia:transaction(fun()-> mnesia:write(#url{code=get_url_code(), url=Url}) end).

-spec get_url(Code::binary()| list()) -> binary().
get_url(Code) when is_list(Code) -> get_url(list_to_binary(Code));
get_url(Code)->
    case mnesia:dirty_read(url, Code) of
        [#url{url=Url}] -> Url;
        [] -> not_found
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%%%%%%%%%%%%%%%%%%%%%
%% Counter functions %%
%%%%%%%%%%%%%%%%%%%%%%%
get_url_code() ->
    list_to_binary(string:to_lower(integer_to_list(bump(url), 36))).

bump(Type) ->
    bump(Type, 1).

bump(Type, Inc) ->
    mnesia:dirty_update_counter(counter, Type, Inc).
