%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2016 11:42
%%%-------------------------------------------------------------------
-module(cache_man_SUITE).
-author("tihon").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gs_conf_headers.hrl").
-include("gs_headers.hrl").
-include("gs_codes.hrl").


all() ->
  [
    test_adding_game,
    test_pulling_game,
    test_double_pulling_game,
    test_pull_first_available_game
  ].

init_per_suite(Config) ->
  Config.

init_per_testcase(_, Config) ->
  application:ensure_all_started(mnesia),
  meck:new(seaconfig),
  meck:expect(seaconfig, get_value, fun(?GAMES) -> <<"testgame">> end),
  gs_cache_man:init(),
  Config.

end_per_testcase(_, Config) ->
  mnesia:stop(),
  Config.

end_per_suite(Config) ->
  Config.


test_adding_game(_) ->
  ct:pal("------------------~p------------------~n", [test_adding_game]),

  Game = <<"testgame">>,
  Vsn = <<"1.0.0">>,
  Uid = <<"some_uid">>,
  Rules = <<"default_rules">>,
  Private = false,
  TTL = <<"5min">>,

  {ok, Gid} = gs_cache_man:add_game(Game, Vsn, Uid, Rules, TTL, Private),

  [#gc_game{rules = Rules, uid = Uid, version = Vsn, gid = Gid}] = mnesia:dirty_read('testgame_game_public', Gid),

  ok.

test_pulling_game(_) ->
  ct:pal("------------------~p------------------~n", [test_pulling_game]),

  Game = <<"testgame">>,
  Vsn = <<"1.0.0">>,
  Uid = <<"some_uid">>,
  Rules = <<"default_rules">>,
  Private = false,
  TTL = <<"5min">>,

  {ok, Gid} = gs_cache_man:add_game(Game, Vsn, Uid, Rules, TTL, Private),

  [#gc_game{rules = Rules, uid = Uid, version = Vsn, gid = Gid}] = mnesia:dirty_read('testgame_game_public', Gid),

  {true, #{?VSN_HEAD := Vsn, ?UID_HEAD := Uid, ?RULES_HEAD := Rules}} = gs_cache_man:pull_game(Gid),

  [] = mnesia:dirty_read('testgame_game_public', Gid),
  ok.

test_double_pulling_game(_) ->
  ct:pal("------------------~p------------------~n", [test_double_pulling_game]),

  Game = <<"testgame">>,
  Vsn = <<"1.0.0">>,
  Uid = <<"some_uid">>,
  Rules = <<"default_rules">>,
  Private = false,
  TTL = <<"5min">>,

  {ok, Gid} = gs_cache_man:add_game(Game, Vsn, Uid, Rules, TTL, Private),

  [#gc_game{rules = Rules, uid = Uid, version = Vsn, gid = Gid}] = mnesia:dirty_read('testgame_game_public', Gid),

  {true, #{?VSN_HEAD := Vsn, ?UID_HEAD := Uid, ?RULES_HEAD := Rules}} = gs_cache_man:pull_game(Gid),

  [] = mnesia:dirty_read('testgame_game_public', Gid),

  {false, ?GAME_STARTED} = gs_cache_man:pull_game(Gid),
  ok.

test_pull_first_available_game(_) ->
  ct:pal("------------------~p------------------~n", [test_pull_first_available_game]),

  Game = <<"testgame">>,
  Vsn = <<"1.0.0">>,
  Uid = <<"some_uid">>,
  Rules = <<"default_rules">>,
  Private = false,
  TTL = <<"5min">>,

  {ok, _} = gs_cache_man:add_game(Game, Vsn, Uid, Rules, TTL, Private),
  {ok, _} = gs_cache_man:add_game(Game, Vsn, Uid, Rules, TTL, Private),
  {ok, _} = gs_cache_man:add_game(Game, Vsn, Uid, Rules, TTL, Private),

  [_, _, _] = mnesia:dirty_select('testgame_game_public', [{#gc_game{gid = '$1', _ = '_'}, [], ['$1']}]),

  {true, #{?GAME_ID_HEAD := _}} = gs_cache_man:pull_first_available_game(Game, Private),

  [_, _] = mnesia:dirty_select('testgame_game_public', [{#gc_game{gid = '$1', _ = '_'}, [], ['$1']}]),

  {true, #{?GAME_ID_HEAD := _}} = gs_cache_man:pull_first_available_game(Game, Private),

  [_] = mnesia:dirty_select('testgame_game_public', [{#gc_game{gid = '$1', _ = '_'}, [], ['$1']}]),

  {true, #{?GAME_ID_HEAD := _}} = gs_cache_man:pull_first_available_game(Game, Private),

  [] = mnesia:dirty_select('testgame_game_public', [{#gc_game{gid = '$1', _ = '_'}, [], ['$1']}]),

  {false, ?GAME_STARTED} = gs_cache_man:pull_first_available_game(Game, Private),

  ok.