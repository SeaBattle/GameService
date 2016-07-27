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
-include("gc_codes.hrl").

-type game_type() :: ?PRIVATE_GAME | ?ORDINARY_GAME.

-export_type([game_type/0]).

%% API
-export([create_game/1, accept_game/1, fast_play/1]).

create_game(Package = #{?VSN_HEAD := Vsn, ?UID_HEAD := Uid, ?TTL_HEAD := TTL}) ->
  Type = get_game_type(Package),
  Gid = generate_id(Vsn, Type),
  Rules = get_rules(Package),
  {ok, <<"OK">>} = gs_cache_man:add_game(Gid, Vsn, Uid, Rules, TTL, Type),
  #{?RESULT_HEAD => true, ?CODE_HEAD => ?OK, ?GAME_ID_HEAD => Gid}.

accept_game(#{?GAME_ID_HEAD := GameId, ?VSN_HEAD := _Vsn, ?UID_HEAD := Uid}) ->
  Type = get_type_from_id(GameId),
  case gs_cache_man:set_lock(GameId, Type, Uid) of
    {true, #{?UID_HEAD := Host, ?RULES_HEAD := Rules}} ->  %lock captured  %TODO check client version support rules (before deleting from list).
      #{?RESULT_HEAD => true, ?CODE_HEAD => ?OK, ?UID_HEAD => Host, ?RULES_HEAD => Rules};
    {false, Reason} ->  %game expired or started
      #{?RESULT_HEAD => false, ?CODE_HEAD => Reason}
  end.

fast_play(Package = #{?VSN_HEAD := Vsn, ?UID_HEAD := Uid, ?TTL_HEAD := TTL}) ->
  case gs_cache_man:get_random_game(?ORDINARY_GAME, Uid) of
    {true, #{?UID_HEAD := Host, ?RULES_HEAD := Rules}} ->  %TODO check client version support rules (before deleting from list).
      #{?RESULT_HEAD => true, ?CODE_HEAD => ?OK, ?UID_HEAD => Host, ?RULES_HEAD => Rules};
    {false, ?GAME_NOT_AVAILABLE} ->
      Gid = generate_id(Vsn, ?ORDINARY_GAME),
      Rules = get_rules(Package),
      {ok, <<"OK">>} = gs_cache_man:add_game(Gid, Vsn, Uid, Rules, TTL, ?ORDINARY_GAME),
      #{?RESULT_HEAD => true, ?CODE_HEAD => ?WAITING_FOR_CONNECT, ?GAME_ID_HEAD => Gid}
  end.


%% @private
get_rules(#{?RULES_HEAD := Rules}) -> Rules;
get_rules(#{?VSN_HEAD := Vsn}) -> get_default_rules(Vsn).

%% @private
get_default_rules(<<"seabattle-", VsnNum/binary>>) -> gs_seabattle:get_default_rules(VsnNum).

%% @private
generate_id(Vsn, Type) ->
  UUID = list_to_binary(uuid:to_string(uuid:uuid4())),
  TypeBin = atom_to_binary(Type, utf8),
  <<TypeBin/binary, <<"_">>/binary, (su_utils:hash_sha256(<<Vsn/binary, UUID/binary>>))/binary>>.

%% @private
get_type_from_id(GameId) ->
  [TypeBin | _] = binary:split(GameId, <<"_">>),
  TypeBin.

%% @private
get_game_type(#{?PRIVATE_HEAD := true}) -> ?PRIVATE_GAME;
get_game_type(_) -> ?ORDINARY_GAME.