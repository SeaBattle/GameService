%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jul 2016 22:11
%%%-------------------------------------------------------------------
-module(gs_cache_man).
-author("tihon").

-include("gc_codes.hrl").
-include("gs_headers.hrl").
-include_lib("seaconfig/include/sc_headers.hrl").

%% API
-export([add_game/5, init/0, set_lock/2]).

init() ->
  ok = application:load(eredis_cluster),
  Nodes = sc_conf_holder:get_conf(?CACHE_HOSTS_CONF, <<"127.0.0.1:30001">>),
  PoolSize = sc_conf_holder:get_conf(?CACHE_SIZE_CONF, 5),
  PoolOverflow = sc_conf_holder:get_conf(?CACHE_OVERFLOW_CONF, 100),
  PoolOverflowTTL = sc_conf_holder:get_conf(?CACHE_OVERFLOW_TTL_CONF, 5000),
  PoolOverflowCheck = sc_conf_holder:get_conf(?CACHE_OVERFLOW_CHECK_CONF, 1000),
  application:set_env(eredis_cluster, init_nodes, form_init_nodes(Nodes)),
  application:set_env(eredis_cluster, pool_size, PoolSize),
  application:set_env(eredis_cluster, pool_max_overflow, PoolOverflow),
  application:set_env(eredis_cluster, overflow_ttl, PoolOverflowTTL),
  application:set_env(eredis_cluster, overflow_check_period, PoolOverflowCheck),
  application:ensure_all_started(eredis_cluster).

-spec add_game(binary(), binary(), binary(), proplists:proplist(), integer()) ->
  {ok, binary()} | {error, binary() | atom()}.
add_game(Gid, Vsn, Uid, Rules, TTL) ->
  {ok, _} = eredis_cluster:q([<<"HSETNX">>, Gid, ?VSN_HEAD, Vsn]),
  {ok, _} = eredis_cluster:q([<<"HMSET">>, Gid, ?UID_HEAD, Uid, ?RULES_HEAD, term_to_binary(Rules)]),
  eredis_cluster:q([<<"EXPIRE">>, Gid, TTL]).

-spec set_lock(binary(), binary()) -> {false, integer()} | {true, map()}.
set_lock(Gid, EnemyUid) ->
  case eredis_cluster:q([<<"HSETNX">>, Gid, ?ENEMY_HEAD, EnemyUid]) of
    {ok, <<"1">>} ->  %lock captured
      {ok, Result} = eredis_cluster:q([<<"HGETALL">>, Gid]),
      {ok, <<"OK">>} = eredis_cluster:q([<<"DEL">>, Gid]),  %delete game
      Map = result_to_map(Result, #{}),
      case maps:is_key(?UID_HEAD, Map) of
        true -> %game valid - return it
          {true, Map};
        false ->  %no game (it was just enemy, set by lock)
          {false, ?GAME_NOT_AVAILABLE}
      end;
    {ok, <<"0">>} ->  %game expired or started
      {false, ?GAME_STARTED}
  end.


%% @private
form_init_nodes(BinHosts) ->
  Hosts = binary:split(BinHosts, <<",">>, [global]),
  lists:foldl(
    fun(Host, A) ->
      [Host, Port] = binary:split(Host, <<":">>),
      [{binary_to_list(Host), binary_to_integer(Port)} | A]
    end, [], Hosts).

%% @private
result_to_map([], Acc) -> Acc;
result_to_map([Key, Value | Rest], Acc) ->
  result_to_map(Rest, Acc#{Key => Value}).