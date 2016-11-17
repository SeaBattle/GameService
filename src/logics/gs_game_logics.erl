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

%% TODO user should not be able to play with himself!
%% Write game to db
create_game(Package = #{?GAME_HEAD := Game, ?VSN_HEAD := Vsn, ?UID_HEAD := Uid, ?TTL_HEAD := TTL}) ->
  Private = maps:get(?PRIVATE_HEAD, Package, false),
  Rules = get_rules(Package),
  {ok, Gid} = gs_cache_man:add_game(Game, Vsn, Uid, Rules, TTL, Private),
  #{?RESULT_HEAD => true, ?CODE_HEAD => ?OK, ?GAME_ID_HEAD => Gid}.

%% TODO user should not be able to play with himself!
join_game(#{?GAME_ID_HEAD := GameId, ?VSN_HEAD := _Vsn}) ->
  case gs_cache_man:pull_game(GameId) of
    {true, Game} ->  %game pulled from list
      Game#{?RESULT_HEAD => true, ?CODE_HEAD => ?OK};
    {false, Reason} ->  %game expired or started
      #{?RESULT_HEAD => false, ?CODE_HEAD => Reason}
  end.

%% TODO user should not be able to play with himself!
fast_play(Package = #{?GAME_HEAD := Game, ?UID_HEAD := Uid, ?TTL_HEAD := TTL, ?VSN_HEAD := Vsn}) ->
  case gs_cache_man:pull_first_available_game(Game, false) of
    {true, GameFound} when is_map(GameFound)->
      GameFound#{?RESULT_HEAD => true, ?CODE_HEAD => ?OK};
    {false, ?GAME_STARTED} -> %no games to join. Should create new.
      Rules = get_rules(Package),
      {ok, Gid}  = gs_cache_man:add_game(Game, Vsn, Uid, Rules, TTL, false),
      #{?RESULT_HEAD => true, ?CODE_HEAD => ?WAITING_FOR_CONNECT, ?GAME_ID_HEAD => Gid}
  end.


%% @private
get_rules(#{?RULES_HEAD := Rules}) -> Rules;
get_rules(#{?VSN_HEAD := Vsn, ?GAME_HEAD := Game}) -> gs_rules_man:get_default_rules(Game, Vsn).