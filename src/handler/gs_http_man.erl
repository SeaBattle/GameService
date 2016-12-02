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

-include("gs_conf_headers.hrl").

%% API
-export([init_http_handler/0]).

init_http_handler() ->
  PortBin = get_default_value(?HTTP_PORT, <<"8080">>),
  AcceptorsBin = get_default_value(?HTTP_ACCEPTORS, <<"100">>),
  Dispatch = cowboy_router:compile(
    [
      {'_',
        [
          {'_', gs_http_handler, []}
        ]
      }
    ]),
  {ok, _} =
    cowboy:start_http(
      http_handler,
      binary_to_integer(AcceptorsBin),
      [{port, binary_to_integer(PortBin)}],
      [{env, [{dispatch, Dispatch}]}]),
  ok.


%% @private
get_default_value(Key, Default) ->
  case seaconfig:get_value(Key) of
    {ok, Value} -> Value;
    {error, _} -> Default
  end.