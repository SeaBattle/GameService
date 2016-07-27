%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jul 2016 22:10
%%%-------------------------------------------------------------------
-module(gs_http_man).
-author("tihon").

%% API
-export([init_http_handler/0]).

init_http_handler() ->
  Port = 8080,  %TODO get configuration
  Acceptors = 100,
  Dispatch = cowboy_router:compile(
    [
      {'_',
        [
          {'_', gs_http_handler, []}
        ]
      }
    ]),
  {ok, _} = cowboy:start_http(http_handler, Acceptors, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
  ok.