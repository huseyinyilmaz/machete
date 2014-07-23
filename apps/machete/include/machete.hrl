%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% header file that holds common types and records in application
%%% @end
%%% Created : 13 Jul 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------

-record(url, {code::binary(),
              url::binary()}).

-record(counter, {name::term(),
                  value::integer()}).
