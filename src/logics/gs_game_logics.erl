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

%% API
-export([create_game/1, accept_game/1, fast_play/1, list_games/1]).

create_game(Package = #{?VSN_HEAD := Vsn, ?UID_HEAD := Uid, ?TTL_HEAD := TTL}) ->
  Gid = generate_id(Vsn),
  Rules = get_rules(Package),
  {ok, <<"OK">>} = gs_cache_man:add_game(Gid, Vsn, Uid, Rules, TTL),
  #{?RESULT_HEAD => true, ?CODE_HEAD => ?OK, ?GAME_ID_HEAD => Gid}.

accept_game(#{?GAME_ID_HEAD := GameId, ?VSN_HEAD := _Vsn, ?UID_HEAD := Uid}) ->
  case gs_cache_man:set_lock(GameId, Uid) of
    {true, #{?UID_HEAD := Host, ?RULES_HEAD := Rules}} ->  %lock captured  %TODO check client version support rules (before deleting from list).
      #{?RESULT_HEAD => true, ?CODE_HEAD => ?OK, ?UID_HEAD => Host, ?RULES_HEAD => Rules};
    {false, Reason} ->  %game expired or started
      #{?RESULT_HEAD => false, ?CODE_HEAD => Reason}
  end.

fast_play(#{?VSN_HEAD := Vsn, ?UID_HEAD := Uid, ?TTL_HEAD := TTL}) ->
  ok.

list_games(#{?VSN_HEAD := Vsn}) ->
  ok.


%% @private
get_rules(#{?RULES_HEAD := Rules}) -> Rules;
get_rules(#{?VSN_HEAD := Vsn}) -> get_default_rules(Vsn).

%% @private
get_default_rules(<<"seabattle-", VsnNum/binary>>) -> gs_seabattle:get_default_rules(VsnNum).

%% @private
generate_id(Vsn) ->
  UUID = list_to_binary(uuid:to_string(uuid:uuid4())),
  su_utils:hash_sha256(<<Vsn/binary, UUID/binary>>).