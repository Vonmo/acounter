%%%-------------------------------------------------------------------
%% @doc acounter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(acounter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(Id, Module, Type, Args), #{id => Id,
                                         start => {Module, start_link, [Args]},
                                         restart => permanent,
                                         shutdown => infinity,
                                         type => Type,
                                         modules => [Module]}).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 1000, 10}, lists:flatten([
        ?CHILD(counters_storage, worker)
    ])} }.

%%====================================================================
%% Internal functions
%%====================================================================
