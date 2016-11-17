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

all() ->
  [
  ].

init_per_suite(Config) ->
  Config.

init_per_testcase(_, Config) ->
  application:ensure_all_started(mnesia),
  gs_cache_man:init(),
  Config.

end_per_testcase(_, Config) ->
  mnesia:stop(),
  Config.

end_per_suite(Config) ->
  Config.