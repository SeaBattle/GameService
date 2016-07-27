%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jul 2016 22:09
%%%-------------------------------------------------------------------
-module(gs_http_handler).
-author("tihon").

%% Cowboy callbacks
-export([init/2, reply/2, reply/3]).

init(Req, Opts) ->
  Req2 = act(cowboy_req:path(Req), Req, Opts),
  {ok, Req2, Opts}.

-spec reply(cowboy_req:req(), integer()) -> cowboy_req:req().
reply(Req, _) ->
  cowboy_req:reply(404, [
    {<<"content-type">>, <<"text/html">>}
  ], <<"Not found">>, Req).

reply(Req, 200, Data) ->
  cowboy_req:reply(200, [
    {<<"content-type">>, <<"application/json">>}
  ], Data, Req).


%% @private
act(<<"/create/", _/binary>>, Req, _) ->
  {ok, Body, Req2} = cowboy_req:body(Req, [{length, infinity}]),
  Decoded = jsone:decode(Body, [{object_format, map}]),
  Result = gs_game_logics:create_game(Decoded),
  reply(Req2, 200, jsone:encode(Result));
act(<<"/connect/", _/binary>>, Req, _) ->
  {ok, Body, Req2} = cowboy_req:body(Req, [{length, infinity}]),
  Decoded = jsone:decode(Body, [{object_format, map}]),
  Result = gs_game_logics:join_game(Decoded),
  reply(Req2, 200, jsone:encode(Result));
act(<<"/play/", _/binary>>, Req, _) ->
  {ok, Body, Req2} = cowboy_req:body(Req, [{length, infinity}]),
  Decoded = jsone:decode(Body, [{object_format, map}]),
  Result = gs_game_logics:fast_play(Decoded),
  reply(Req2, 200, jsone:encode(Result));
act(_, Req, _) ->
  reply(Req, 404).