%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2016 19:57
%%%-------------------------------------------------------------------
-module(gs_rules_man).
-author("tihon").

%% API
-export([get_default_rules/2]).

-spec get_default_rules(binary(), binary()) -> binary().
get_default_rules(_, _) ->  %TODO get default rules from consul by service?
  term_to_binary([
    {<<"shipNum">>, 10},
    {<<"ship1">>, 4},
    {<<"ship2">>, 3},
    {<<"ship3">>, 2},
    {<<"ship4">>, 1},
    {<<"hitPerTurn">>, 1},
    {<<"repeatOnHit">>, true},
    {<<"allowNearPlace">>, false}
  ]).