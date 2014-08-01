%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Library file that holds mnesia records.
%%% @end
%%% Created : 29 Jul 2014 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
-record(url, {code::binary(),
              url::binary()}).

-record(counter, {name::term(),
                  value::integer()}).
