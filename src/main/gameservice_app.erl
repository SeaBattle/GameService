-module(gameservice_app).

-behaviour(application).

-include_lib("seaconfig/include/sc_headers.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Ret = su_super_sup:start_link(),
  {ok, _} = su_cache_man:init(),
  Ret.

stop(_State) ->
  ok.