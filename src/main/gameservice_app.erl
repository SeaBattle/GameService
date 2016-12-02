-module(gameservice_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Ret = su_super_sup:start_link(),
  ok = join_cluster(),
  {ok, _} = gs_cache_man:init(),
  ok = gs_http_man:init_http_handler(),
  Ret.

stop(_State) ->
  ok.


%% @private
join_cluster() ->
  Services = seaconfig:get_service("game_service"),
  lists:foreach(fun(#{<<"Address">> := Addr}) -> net_adm:ping(Addr) end, Services).