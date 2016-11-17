%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2016 11:29
%%%-------------------------------------------------------------------
-module(game_id_tests).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").

generate_to_different_id_test() ->
  Id1 = gs_game_id_man:generate_id(<<"game">>, <<"1.0.0">>, true),
  Id2 = gs_game_id_man:generate_id(<<"game">>, <<"1.0.0">>, true),
  ?assertNotEqual(Id1, Id2).

get_game_and_private_test() ->
  Id1 = gs_game_id_man:generate_id(<<"game">>, <<"1.0.0">>, true),
  {Game, Id} = gs_game_id_man:get_game_n_type_from_id(Id1),
  ?assertEqual(Game, <<"game">>),
  ?assertEqual(Id, true).
