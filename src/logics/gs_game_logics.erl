%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jul 2016 22:07
%%%-------------------------------------------------------------------
-module(gs_game_logics).
-author("tihon").

-include("gs_headers.hrl").
-include("gs_codes.hrl").

%% API
-export([create_game/1, join_game/1, fast_play/1]).

%% Write game to db
create_game(Package = #{?GAME_HEAD := Game, ?VSN_HEAD := Vsn, ?UID_HEAD := Uid, ?TTL_HEAD := TTL}) ->
  Private = maps:get(?PRIVATE_HEAD, Package, false),
  Gid = gs_game_id_man:generate_id(Game, Vsn, Private),
  Rules = get_rules(Package),
  Res = ok =:= gs_cache_man:add_game(Game, Gid, Vsn, Uid, Rules, TTL, Private),
  #{?RESULT_HEAD => Res, ?CODE_HEAD => ?OK, ?GAME_ID_HEAD => Gid}.

join_game(#{?GAME_ID_HEAD := GameId, ?VSN_HEAD := _Vsn}) ->
  case gs_cache_man:pull_game(GameId) of
    {true, #{?UID_HEAD := Host, ?RULES_HEAD := Rules}} ->  %game pulled from list
      #{?RESULT_HEAD => true, ?CODE_HEAD => ?OK, ?UID_HEAD => Host, ?RULES_HEAD => Rules};
    {false, Reason} ->  %game expired or started
      #{?RESULT_HEAD => false, ?CODE_HEAD => Reason}
  end.

fast_play(Package = #{?GAME_HEAD := Game, ?UID_HEAD := Uid, ?TTL_HEAD := TTL, ?VSN_HEAD := Vsn}) ->
  case gs_cache_man:pull_first_available_game(Game, false) of
    {true, Game} when is_map(Game)->
      Game#{?RESULT_HEAD => true, ?CODE_HEAD => ?OK};
    {false, ?GAME_NOT_AVAILABLE} -> %no games to join. Should create new.
      Gid = gs_game_id_man:generate_id(Game, Vsn, false),
      Rules = get_rules(Package),
      ok = gs_cache_man:add_game(Game, Gid, Vsn, Uid, Rules, TTL, false),
      #{?RESULT_HEAD => true, ?CODE_HEAD => ?WAITING_FOR_CONNECT, ?GAME_ID_HEAD => Gid}
  end.


%% @private
get_rules(#{?RULES_HEAD := Rules}) -> Rules;
get_rules(#{?VSN_HEAD := Vsn, ?GAME_HEAD := Game}) -> gs_rules_man:get_default_rules(Game, Vsn).