-module(counter_handler).

-export([init/2]).
-export([terminate/3]).

%% includes
-include_lib("eunit/include/eunit.hrl").

-define(TABLE_NAME, counters_storage).


init(Req, State) ->
    handle(proplists:get_value(m, State, undef), Req, State).

terminate(_Reason, _Req, _State) -> ok.

handle(increase, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    Req2 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            jiffy:encode(#{code => 200}),
                            Req),
    ets:update_counter(?TABLE_NAME, Id, {2, 1}, {Id, 0}),
    metrix:increase_counter("web.requests.http.increase"),
    {ok, Req2, State};

handle(decrease, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    Req2 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            jiffy:encode(#{code => 200}),
                            Req),
    ets:update_counter(?TABLE_NAME, Id, {2, -1}, {Id, 0}),
    metrix:increase_counter("web.requests.http.decrease"),
    {ok, Req2, State};

handle(get, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    Val = case ets:lookup(?TABLE_NAME, Id) of
        [{Id, Cur}|_] -> Cur;
        _Else -> 0
    end,
    Req2 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            jiffy:encode(#{code => 200, value => Val}),
                            Req),
    metrix:increase_counter("web.requests.http.get"),
    {ok, Req2, State};

handle(reset, Req, State) ->
    Id = cowboy_req:binding(id, Req, unknown),
    Req2 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            jiffy:encode(#{code => 200}),
                            Req),
    ets:delete(?TABLE_NAME, Id),
    metrix:increase_counter("web.requests.http.reset"),
    {ok, Req2, State};

handle(UnknownMethod, Req, State) ->
    Req2 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            jiffy:encode(#{code => 400, method => UnknownMethod}),
                            Req),
    metrix:increase_counter("web.requests.http.400"),
    {ok, Req2, State}.
