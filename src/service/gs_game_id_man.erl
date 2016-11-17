%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2016 11:30
%%%-------------------------------------------------------------------
-module(gs_game_id_man).
-author("tihon").

-type game_id() :: binary().

-export_type([game_id/0]).

%% API
-export([generate_id/3, get_game_n_type_from_id/1]).

-spec generate_id(binary(), binary(), boolean()) -> game_id().
generate_id(Game, Vsn, Private) ->
  UUID = list_to_binary(uuid:to_string(uuid:uuid4())),
  TypeBin = atom_to_binary(Private, utf8),
  <<Game/binary, <<"_">>/binary, TypeBin/binary, <<"_">>/binary, (su_utils:hash_sha256(<<Vsn/binary, UUID/binary>>))/binary>>.

-spec get_game_n_type_from_id(game_id()) -> {binary(), boolean()}.
get_game_n_type_from_id(GameId) ->
  [Game, TypeBin | _] = binary:split(GameId, <<"_">>, [global]),
  {Game, binary_to_existing_atom(TypeBin, utf8)}.