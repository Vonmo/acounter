-module(metrix_system_agent).

-behaviour(gen_server).

-author('Maxim Molchanov <mr.elzor@gmail.com>').

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% includes

%% defines

%% records
-record(state, {timeout :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(_Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, RefreshTimeout} = application:get_env(metrix, refresh_timeout_ms),
    statistics(reductions), % Initialize reductions
    gen_server:cast(self(), update_metrix),
    {ok, #state{timeout = RefreshTimeout}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(update_metrix, #state{timeout = Timeout} = State) ->
    put_system_metrix(),
    erlang:send_after(Timeout, self(), {'$gen_cast', update_metrix}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
put_system_metrix() ->
    % Get system metrix
    MemoryTotal          = erlang:memory(total),
    MemoryProcesses      = erlang:memory(processes),
    MemoryProcessesUsed  = erlang:memory(processes_used),
    MemoryBinary         = erlang:memory(binary),
    MemoryEts            = erlang:memory(ets),
    MemorySystem         = erlang:memory(system),
    ProcCount            = length(processes()),
    {_, DeltaReductions} = statistics(reductions),
    RunQueue             = statistics(run_queue),
    Nodes                = nodes(),
    {{input, IOInput},
     {output, IOOutput}}  = erlang:statistics(io),

    % Put system metrix
    metrix:put_value("erlang.memory.total", MemoryTotal),
    metrix:put_value("erlang.memory.system", MemorySystem),
    metrix:put_value("erlang.memory.processes", MemoryProcesses),
    metrix:put_value("erlang.memory.processes_used", MemoryProcessesUsed),
    metrix:put_value("erlang.memory.binary", MemoryBinary),
    metrix:put_value("erlang.memory.ets", MemoryEts),
    metrix:put_value("erlang.processes.count", ProcCount),
    metrix:put_value("erlang.processes.run_queue", RunQueue),
    metrix:put_value("erlang.reductions", DeltaReductions),
    metrix:put_value("erlang.io.input", IOInput),
    metrix:put_value("erlang.io.output", IOOutput),
    metrix:put_value("erlang.nodes", Nodes),
    metrix:put_value("erlang._time", cur_time()),
    ok.

cur_time() ->
    {{Y, Mon, D}, {H, Min, S}} = calendar:local_time(),
    lists:flatten(
        io_lib:format("~4.4.0w.~2.2.0w.~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                      [Y, Mon, D, H, Min, S])
    ).
