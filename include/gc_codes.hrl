%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2016 20:06
%%%-------------------------------------------------------------------
-author("tihon").

-define(OK, 0).

-define(GAME_STARTED, 100).       %other player just joined and game starts
-define(GAME_NOT_AVAILABLE, 101). %no game for such GID was found
-define(WAITING_FOR_CONNECT, 102).  %fast play is not able to find ready game, so it creates game and wait for connection

-define(SERVER_ERROR, 500).