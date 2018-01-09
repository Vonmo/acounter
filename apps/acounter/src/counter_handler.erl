-module(counter_handler).

-export([init/2]).
-export([terminate/3]).

%% includes
-include_lib("eunit/include/eunit.hrl").

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
    counters_storage:increase(Id),
    {200, #{code => 200}, Req, State};

handle(decrease, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    counters_storage:decrease(Id),
    {200, #{code => 200}, Req, State};

handle(get, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    {200, #{code => 200, value => counters_storage:get(Id)}, Req, State};

handle(reset, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    counters_storage:reset(Id),
    {200, #{code => 200}, Req, State};

handle(UnknownMethod, Req, State) ->
    metrix:increase_counter("web.requests.http.400"),
    {400, #{code => 400, method => UnknownMethod}, Req, State}.
