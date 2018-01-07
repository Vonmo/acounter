%%%-------------------------------------------------------------------
%% @doc acounter public API
%% @end
%%%-------------------------------------------------------------------

-module(acounter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    % init cowboy
    {ok, Dispatch} = application:get_env(acounter, dispatch),
    {ok, #{port := Port}} = application:get_env(acounter, socket),
    {ok, _} = cowboy:start_clear(http, [{port, Port}, {max_connections, infinity}], #{
        env => #{
            dispatch => cowboy_router:compile(Dispatch)
        },
        middlewares => [
            cowboy_router,
            cowboy_handler
        ]
    }),
    acounter_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
