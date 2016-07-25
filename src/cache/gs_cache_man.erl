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

-include("gs_headers.hrl").
-include_lib("seaconfig/include/sc_headers.hrl").

%% API
-export([add_game/5, init/0]).

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
  {ok, _} = eredis_cluster:q([<<"HSETNX">>, Gid, ?UID_HEAD, Uid]),
  {ok, _} = eredis_cluster:q([<<"HSETNX">>, Gid, ?RULES_HEAD, term_to_binary(Rules)]),
  eredis_cluster:q([<<"EXPIRE">>, Gid, TTL]).

%% @private
form_init_nodes(BinHosts) ->
  Hosts = binary:split(BinHosts, <<",">>, [global]),
  lists:foldl(
    fun(Host, A) ->
      [Host, Port] = binary:split(Host, <<":">>),
      [{binary_to_list(Host), binary_to_integer(Port)} | A]
    end, [], Hosts).