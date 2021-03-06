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
-export([add_game/6, init/0, pull_game/1, pull_first_available_game/2]).

-spec init() -> undefined | error | ok.
init() ->
  case seaconfig:get_value(?GAMES) of
    undefined -> undefined;
    {error, _} -> error;
    GameBin ->
      GameList = binary:split(GameBin, <<",">>, [global]),
      create_tables(GameList)
  end.

-spec add_game(binary(), binary(), binary(), proplists:proplist(), integer(), boolean()) ->
  {ok, gs_game_id_man:game_id()} | {error, any()}.
add_game(Game, Vsn, Uid, Rules, _TTL, Private) -> %TODO TTL?
  Gid = gs_game_id_man:generate_id(Game, Vsn, Private),
  Name = binary_to_existing_atom(compose_name(Game, Private), utf8),
  try mnesia:dirty_write(Name, #gc_game{gid = Gid, rules = Rules, uid = Uid, version = Vsn}) of
    ok -> {ok, Gid};
    Other -> {error, Other}
  catch
    _:Reason -> {error, Reason}
  end.

-spec pull_game(gs_game_id_man:game_id()) -> {false, integer()} | {true, map()}.
pull_game(GameId) ->
  {Game, Private} = gs_game_id_man:get_game_n_type_from_id(GameId),
  Name = binary_to_existing_atom(compose_name(Game, Private), utf8),
  case mnesia:transaction(fun() -> do_pull_game(Name, GameId) end) of
    {aborted, _} -> {false, ?GAME_NOT_AVAILABLE};
    {atomic, undefined} -> {false, ?GAME_STARTED};
    {atomic, Map} when is_map(Map) -> {true, Map}
  end.

-spec pull_first_available_game(binary(), boolean()) -> {false, integer()} | {true, map()}.
pull_first_available_game(Game, Private) ->
  Name = binary_to_existing_atom(compose_name(Game, Private), utf8),
  case mnesia:transaction(fun() -> do_pull_first_game(Name) end) of
    {aborted, _} -> {false, ?GAME_NOT_AVAILABLE};
    {atomic, undefined} -> {false, ?GAME_STARTED};
    {atomic, Map} when is_map(Map) -> {true, Map}
  end.


%% @private
%% Need to be run in transaction.
do_pull_game(Name, Gid) ->
  case mnesia:read(Name, Gid, write) of
    [#gc_game{version = Vsn, uid = Uid, rules = Rules}] ->
      ok = mnesia:delete(Name, Gid, write),
      #{?VSN_HEAD => Vsn, ?UID_HEAD => Uid, ?RULES_HEAD => Rules, ?GAME_ID_HEAD => Gid};
    [] -> undefined
  end.

%% @private
%% Need to be run in transaction.
do_pull_first_game(Name) ->
  Gid = mnesia:first(Name),
  do_pull_game(Name, Gid).

%% @private
create_tables(GameList) ->
  AllNodes = [node() | nodes()],
  mnesia:change_config(extra_db_nodes, AllNodes),
  lists:foreach(fun(Game) -> ok = create_table(Game, AllNodes) end, GameList).

%% @private
create_table(Game, Nodes) ->
  ProperName = binary:replace(Game, <<" ">>, <<>>, [global]),
  PrivateGameName = binary_to_atom(compose_name(ProperName, true), utf8),
  PublicGameName = binary_to_atom(compose_name(ProperName, false), utf8),
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
compose_name(Game, true) -> <<Game/binary, <<"_game_private">>/binary>>;
compose_name(Game, false) -> <<Game/binary, <<"_game_public">>/binary>>.

%% @private
get_opts(Nodes) ->
  [
    {record_name, gc_game},
    {type, set},
    {ram_copies, Nodes},
    {attributes, record_info(fields, gc_game)},
    {index, [#gc_game.rules]},
    {storage_properties, [{ets, [{read_concurrency, true}, {write_concurrency, true}]}]}
  ].