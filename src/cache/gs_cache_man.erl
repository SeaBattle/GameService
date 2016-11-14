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

-include("gs_codes.hrl").
-include("gs_headers.hrl").
-include("gs_conf_headers.hrl").

-define(WAIT_FOR_TABLES, 60000).

%% API
-export([add_game/6, init/0, set_lock/3, get_random_game/2]).

init() ->
  GameString = seaconfig:get_value(?GAMES),
  GameList = string:tokens(GameString, ","),
  create_tables(GameList).

-spec add_game(binary(), binary(), binary(), proplists:proplist(), integer(), gs_game_logics:game_type()) ->
  {ok, binary()} | {error, binary() | atom()}.
add_game(Gid, Vsn, Uid, Rules, TTL, GameType) ->
  Db = select_database(GameType),
  Result = run_on_proper_base(Db,
    [<<"HMSET">>, Gid, ?VSN_HEAD, Vsn, ?UID_HEAD, Uid, ?RULES_HEAD, term_to_binary(Rules)]),
  case Result of
    {ok, <<"OK">>} -> run_on_proper_base(Db, [<<"EXPIRE">>, Gid, TTL]);
    Error -> Error
  end.

-spec set_lock(binary(), gs_game_logics:game_type(), binary()) -> {false, integer()} | {true, map()}.
set_lock(Gid, Type, EnemyUid) ->
  Db = select_database(Type),
  case run_on_proper_base(Db, [<<"HSETNX">>, Gid, ?ENEMY_HEAD, EnemyUid]) of
    {ok, <<"1">>} ->  %lock captured
      {ok, Result} = run_on_proper_base(Db, [<<"HGETALL">>, Gid]),
      {ok, <<"OK">>} = run_on_proper_base(Db, [<<"DEL">>, Gid]),  %delete game
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

-spec get_random_game(gs_game_logics:game_type(), binary()) -> {false, integer()} | {true, map()}.
get_random_game(Type, Uid) ->
  case run_on_proper_base(Type, [<<"RANDOMKEY">>]) of
    {ok, undefined} -> {false, ?GAME_NOT_AVAILABLE};
    {ok, Key} ->  %game found - try to capture the lock
      case set_lock(Key, Type, Uid) of  %TODO will this be slow? May be some atomic locking?
        {false, _} -> get_random_game(Type, Uid);
        {true, Game} -> {true, Game}
      end
  end.


%% @private
create_tables(GameList) ->
  AllNodes = [node() | nodes()],
  mnesia:change_config(extra_db_nodes, AllNodes),
  lists:foreach(fun(Game) -> ok = create_table(Game, AllNodes) end, GameList).

%% @private
create_table(Game, Nodes) ->
  ProperName = string:strip(Game),
  PrivateGameName = get_table_name(ProperName, true),
  PublicGameName = get_table_name(ProperName, false),
  ok = do_create_table(PrivateGameName, get_opts(Nodes)),
  ok = do_create_table(PublicGameName, get_opts(Nodes)).

%% @private
-spec do_create_table(atom(), proplists:proplist()) -> ok | {error, any()}.
do_create_table(Table, Options) ->
  CurrentNode = node(),
  %% ensure table exists
  case mnesia:create_table(Table, Options) of
    {atomic, ok} -> ok;
    {aborted, {already_exists, TableName}} ->      %% table already exists, try to add current node as copy
      add_table_copy_to_current_node(TableName);
    {aborted, {already_exists, TableName, CurrentNode}} ->      %% table already exists, try to add current node as copy
      add_table_copy_to_current_node(TableName);
    Other -> {error, Other}
  end.

%% @private
add_table_copy_to_current_node(TableName) ->
  Node = node(),
  mnesia:wait_for_tables([TableName], ?WAIT_FOR_TABLES),  %% wait for table
  case mnesia:add_table_copy(TableName, Node, ram_copies) of  %% add copy
    {atomic, ok} -> ok;
    {aborted, {already_exists, TableName}} -> ok;
    {aborted, {already_exists, TableName, Node}} -> ok;
    {aborted, Reason} -> {error, Reason}
  end.

%% @private
-spec get_table_name(binary(), boolean()) -> atom().
get_table_name(Game, true) ->
  binary_to_atom(<<Game/binary, <<"_game_private">>/binary>>, utf8);
get_table_name(Game, false) ->
  binary_to_atom(<<Game/binary, <<"_game_public">>/binary>>, utf8).

%% @private
get_opts(Nodes) ->
  [
    {type, set},
    {ram_copies, Nodes},
    {attributes, record_info(fields, gc_game)},
    {index, [#gc_game.rules]},
    {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
  ].