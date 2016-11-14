%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2016 19:44
%%%-------------------------------------------------------------------
-author("tihon").

-define(GAME_ID_HEAD, <<"gid">>).
-define(VSN_HEAD, <<"vsn">>).
-define(UID_HEAD, <<"uid">>).
-define(RULES_HEAD, <<"rules">>).
-define(TTL_HEAD, <<"ttl">>).
-define(ENEMY_HEAD, <<"enemy">>).
-define(PRIVATE_HEAD, <<"private">>).
-define(LIMIT_HEAD, <<"limit">>).

-define(RESULT_HEAD, <<"result">>).
-define(CODE_HEAD, <<"code">>).

-define(PRIVATE_GAME, <<"private">>).
-define(ORDINARY_GAME, <<"ordinary">>).

-record(gc_game,
{
  gid :: binary(),
  uid :: binary(),
  rules :: binary(),
  version :: binary()
}).