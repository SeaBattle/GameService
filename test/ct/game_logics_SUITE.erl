%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2016 11:42
%%%-------------------------------------------------------------------
-module(game_logics_SUITE).
-author("tihon").

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gs_conf_headers.hrl").
-include("gs_headers.hrl").
-include("gs_codes.hrl").

-define(GAME, <<"testgame">>).

all() ->
  [
    test_create_game,
    test_join_game,
    test_fast_play,
    test_fast_play_no_games
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

%% User creates a game and it exists in mnesia.
test_create_game(_) ->
  ct:pal("------------------~p------------------~n", [test_create_game]),

  Uid = <<"uid">>,
  TTL = <<"5sec">>,
  Vsn = <<"1.0.0">>,

  #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?GAME_ID_HEAD := Gid} =
    gs_game_logics:create_game(#{?GAME_HEAD => ?GAME, ?VSN_HEAD => Vsn, ?UID_HEAD => Uid, ?TTL_HEAD => TTL}),

  DefaultRules = gs_rules_man:get_default_rules(?GAME, Vsn),

  [#gc_game{rules = DefaultRules, uid = Uid, version = Vsn, gid = Gid}] = mnesia:dirty_read('testgame_game_public', Gid),
  ok.

%% First user creates a public game and second connects to it via game id.
%% No other connections are allowed for this game id after.
test_join_game(_) ->
  ct:pal("------------------~p------------------~n", [test_join_game]),

  Uid = <<"uid">>,
  TTL = <<"5sec">>,
  Vsn = <<"1.0.0">>,

  #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?GAME_ID_HEAD := Gid} =
    gs_game_logics:create_game(#{?GAME_HEAD => ?GAME, ?VSN_HEAD => Vsn, ?UID_HEAD => Uid, ?TTL_HEAD => TTL}),

  DefaultRules = gs_rules_man:get_default_rules(?GAME, Vsn),

  [#gc_game{rules = DefaultRules, uid = Uid, version = Vsn, gid = Gid}] = mnesia:dirty_read('testgame_game_public', Gid),

  #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?UID_HEAD := Uid, ?RULES_HEAD := DefaultRules} =
    gs_game_logics:join_game(#{?GAME_ID_HEAD => Gid, ?VSN_HEAD => Vsn}),

%%  game can't be joined twice
  [] = mnesia:dirty_read('testgame_game_public', Gid),

  #{?RESULT_HEAD := false, ?CODE_HEAD := ?GAME_STARTED} = gs_game_logics:join_game(#{?GAME_ID_HEAD => Gid, ?VSN_HEAD => Vsn}),

  ok.

%% First user creates a public game and second user connects to it via fast_play
test_fast_play(_) ->
  ct:pal("------------------~p------------------~n", [test_fast_play]),

  Uid = <<"uid">>,
  TTL = <<"5sec">>,
  Vsn = <<"1.0.0">>,

  #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?GAME_ID_HEAD := Gid} =
    gs_game_logics:create_game(#{?GAME_HEAD => ?GAME, ?VSN_HEAD => Vsn, ?UID_HEAD => Uid, ?TTL_HEAD => TTL}),

  DefaultRules = gs_rules_man:get_default_rules(?GAME, Vsn),

  [#gc_game{rules = DefaultRules, uid = Uid, version = Vsn, gid = Gid}] = mnesia:dirty_read('testgame_game_public', Gid),

  #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?UID_HEAD := Uid, ?RULES_HEAD := DefaultRules} =
    gs_game_logics:fast_play(#{?GAME_HEAD => ?GAME, ?UID_HEAD => Uid, ?TTL_HEAD => TTL, ?VSN_HEAD => Vsn}),

  [] = mnesia:dirty_read('testgame_game_public', Gid),
  ok.

%% User want's play fast, but no games available. So game is created in public table and gid is returned to user.
%% Any other fast_play request gets this game.
test_fast_play_no_games(_) ->
  ct:pal("------------------~p------------------~n", [test_fast_play_no_games]),

  Uid = <<"uid">>,
  TTL = <<"5sec">>,
  Vsn = <<"1.0.0">>,

  #{?RESULT_HEAD := true, ?CODE_HEAD := ?WAITING_FOR_CONNECT, ?GAME_ID_HEAD := Gid} =
    gs_game_logics:fast_play(#{?GAME_HEAD => ?GAME, ?UID_HEAD => Uid, ?TTL_HEAD => TTL, ?VSN_HEAD => Vsn}),

  [#gc_game{uid = Uid, version = Vsn, gid = Gid}] = mnesia:dirty_read('testgame_game_public', Gid),

  #{?RESULT_HEAD := true, ?CODE_HEAD := ?OK, ?UID_HEAD := Uid} =
    gs_game_logics:fast_play(#{?GAME_HEAD => ?GAME, ?UID_HEAD => Uid, ?TTL_HEAD => TTL, ?VSN_HEAD => Vsn}),

  [] = mnesia:dirty_read('testgame_game_public', Gid),

  ok.