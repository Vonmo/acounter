%%%-------------------------------------------------------------------
%% @doc acounter public API
%% @end
%%%-------------------------------------------------------------------

-module(acounter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    acounter_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================