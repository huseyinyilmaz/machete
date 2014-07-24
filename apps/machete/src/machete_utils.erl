%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 23 Jul 2014 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(machete_utils).

%% API
-export([normalize_url/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% if url does not start with http, https or ftp add an http:// at the
%% begining.
%% @end
%%--------------------------------------------------------------------
normalize_url(<< $h, $t, $t, $p, $:, $/, $/, _/binary >>=Url) -> Url;
normalize_url(<< $h, $t, $t, $p, $s, $:, $/, $/, _/binary >>=Url) -> Url;
normalize_url(<< $f, $t, $p, $:, $/, $/, _/binary >>=Url) -> Url;
normalize_url(Url) -> << <<"http://">>/binary, Url/binary>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================
