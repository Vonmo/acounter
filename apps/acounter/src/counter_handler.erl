-module(counter_handler).

-export([init/2]).
-export([terminate/3]).

%% includes
-include_lib("eunit/include/eunit.hrl").

-define(TABLE_NAME, counters_storage).


init(Req, State) ->
    Method = proplists:get_value(m, State, undef),
    {Status, Body, Req1, State1} = handle(Method, Req, State),
    Req2 = cowboy_req:reply(Status,
                            #{<<"content-type">> => <<"application/json">>},
                            jiffy:encode(Body),
                            Req1),
    metrix:increase_counter("web.requests.http." ++ atom_to_list(Method)),
    {ok, Req2, State1}.

terminate(_Reason, _Req, _State) -> ok.

handle(increase, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    ets:update_counter(?TABLE_NAME, Id, {2, 1}, {Id, 0}),
    {200, #{code => 200}, Req, State};

handle(decrease, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    ets:update_counter(?TABLE_NAME, Id, {2, -1}, {Id, 0}),
    {200, #{code => 200}, Req, State};

handle(get, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    Val = case ets:lookup(?TABLE_NAME, Id) of
        [{Id, Cur}|_] -> Cur;
        _Else -> 0
    end,
    {200, #{code => 200, value => Val}, Req, State};

handle(reset, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    ets:delete(?TABLE_NAME, Id),
    {200, #{code => 200}, Req, State};

handle(UnknownMethod, Req, State) ->
    metrix:increase_counter("web.requests.http.400"),
    {400, #{code => 400, method => UnknownMethod}, Req, State}.
