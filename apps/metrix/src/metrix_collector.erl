-module(metrix_collector).

-behaviour(gen_server).

-author('Maxim Molchanov <mr.elzor@gmail.com>').

%% API
-export([start_link/1, put/2, get/1, increase/1, increase/2,
         decrease/1, decrease/2, test_clear_all/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% includes

%% defines
-define(TABLE_NAME, ?MODULE).

%% records
-record(state, {filename :: string(),
                timeout  :: non_neg_integer()}).

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

put(Name, Value) ->
    gen_server:cast(?MODULE, {put, Name, Value}).

get(Name) ->
    gen_server:cast(?MODULE, {get, Name, self()}).

increase(Name) ->
    gen_server:cast(?MODULE, {increase, Name, 1}).

decrease(Name) ->
    gen_server:cast(?MODULE, {decrease, Name, 1}).

increase(Name, Number) ->
    gen_server:cast(?MODULE, {increase, Name, Number}).

decrease(Name, Number) ->
    gen_server:cast(?MODULE, {decrease, Name, Number}).

test_clear_all() ->
    gen_server:cast(?MODULE, {test_clear_all}).


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
    ets:new(?TABLE_NAME, [named_table, ordered_set, protected]),
    {ok, ResultPath}     = application:get_env(metrix, result_path),
    filelib:ensure_dir(ResultPath),
    {ok, RefreshTimeout} = application:get_env(metrix, refresh_timeout_ms),
    FileName = ResultPath ++ get_nodename_without_hostname(node()),
    gen_server:cast(self(), update_result_file),
    {ok, #state{filename = FileName, timeout = RefreshTimeout}}.

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
handle_cast({put, Name, Value}, State) ->
    ets:insert(?TABLE_NAME, {Name, Value}),
    {noreply, State};

handle_cast({increase, Name, Number}, State) ->
    update_counter(Name, Number),
    {noreply, State};

handle_cast({decrease, Name, Number}, State) ->
    update_counter(Name, -Number),
    {noreply, State};

handle_cast({get, Name, ClientPid}, State) ->
    MetrixValue = case ets:lookup(?TABLE_NAME, Name) of
        [{Name, Value}] -> Value;
        _Else           -> undefined
    end,
    erlang:send(ClientPid, {metrix, Name, MetrixValue}),
    {noreply, State};

handle_cast(update_result_file, #state{filename = FileName,
                                       timeout  = Timeout} = State) ->
    write_mertix(FileName),
    erlang:send_after(Timeout, self(), {'$gen_cast', update_result_file}),
    {noreply, State};

handle_cast({test_clear_all}, State) ->
    ets:delete_all_objects(?TABLE_NAME),
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
get_nodename_without_hostname(Node) ->
    lists:takewhile(fun($@) -> false;
                       (_C) -> true
                    end,
                    atom_to_list(Node)).

write_mertix(FileName) ->
    ListOfLines = ets:foldr(
        fun({Name, Value}, Result) ->
            ResVal = case misc_supp:is_string(Value) of
                true  -> io_lib:format(" = ~s", [Value]);
                false -> io_lib:format(" = ~32768p", [Value])
            end,
            [Name ++ ResVal | Result]
        end,
        [],
        ?TABLE_NAME),
    StrData = lists:flatten(string:join(ListOfLines, "\n")),
    BinData = erlang:list_to_binary(StrData),
    case file:write_file(FileName, BinData) of
        ok    -> ok;
        _Error -> ok
    end.

update_counter(Name, Value) ->
    case ets:member(?TABLE_NAME, Name) of
        true  -> ets:update_counter(?TABLE_NAME, Name, Value);
        false -> ets:insert(?TABLE_NAME, {Name, Value})
    end.
