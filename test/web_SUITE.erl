-module(web_SUITE).
-compile([export_all, nowarn_unused_function, nowarn_export_all]).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        {group, web_common},
        {group, web_metrics},
        {group, web_api}
    ].

groups() ->
    [
        {web_common,
            [parallel, shuffle],
                [setup_web_port, index]},
        {web_metrics,
            [],
                [index_metrics]},

        {web_api,
            [parallel, shuffle],
                [increase, decrease, reset, perf]}
    ].


%% =============================================================================
%% init
%% =============================================================================
init_per_suite(_)->
    application:ensure_all_started(hackney, temporary),
    PoolName = test_pool,
    Options = [{timeout, 150000}, {max_connections, 200}],
    ok = hackney_pool:start_pool(PoolName, Options),
    [].

init_per_group(_Group, Config) ->
    application:stop(cowboy),
    application:stop(ranch),
    ok = application:load(acounter),
    {ok, _} = application:ensure_all_started(acounter, temporary),
    [{init, true} | Config].


%% =============================================================================
%% end
%% =============================================================================
end_per_suite(_)->
    application:stop(hackney),
    [].

end_per_group(_Group, _Config) ->
    ok = application:stop(acounter),
    ok = application:unload(acounter),
    application:stop(metrix),
    application:stop(cowboy),
    application:stop(ranch),
    ok.

%% =============================================================================
%% group: web_common
%% =============================================================================
setup_web_port(_)->
    Uri = base_url(),
    {Status, Code, _Headers, _Ref} = hackney:request(get, Uri, [], <<>>, []),
    {ok, 200} = {Status, Code},
    ok.

index(_)->
    Uri = base_url(),
    {ok, 200, _Headers, Ref} = hackney:request(get, Uri, [], <<>>, []),
    {ok, <<"The little engine that could.">>} = hackney:body(Ref),
    ok.

%% =============================================================================
%% group: web_metrics
%% =============================================================================
index_metrics(_)->
    Uri = base_url(),
    {ok, 200, _Headers, _Ref} = hackney:request(get, Uri, [], <<>>, []),
    1 = metrix:get_value("web.requests.http.index.200"),
    ok.

%% =============================================================================
%% group: web_api
%% =============================================================================
increase(_)->
    Id = uuid:uuid_to_string(uuid:get_v4()),

    0 = getValue(Id),
    increaseValue(Id),
    1 = getValue(Id),
    increaseValue(Id),
    2 = getValue(Id),
    increaseValue(Id),
    3 = getValue(Id),

    ok.

decrease(_)->
    Id = uuid:uuid_to_string(uuid:get_v4()),

    0 = getValue(Id),
    increaseValue(Id),
    1 = getValue(Id),
    increaseValue(Id),
    2 = getValue(Id),
    decreaseValue(Id),
    1 = getValue(Id),
    decreaseValue(Id),
    0 = getValue(Id),

    ok.

reset(_)->
    Id = uuid:uuid_to_string(uuid:get_v4()),

    0 = getValue(Id),
    increaseValue(Id),
    1 = getValue(Id),
    resetValue(Id),
    0 = getValue(Id),

    ok.

perf(_)->
    R = perftest:comprehensive(1000, fun()->
        Id = uuid:uuid_to_string(uuid:get_v4()),
        increaseValue(Id),
        1 = getValue(Id)
    end),
    true = lists:all(fun(E)-> E >=500 end, R),
    ok.

%% =============================================================================
base_url()->
    #{port:=Port} = application:get_env(acounter, socket, #{}),
    "http://localhost:"++integer_to_list(Port)++"/".

increaseValue(Id)->
    Uri = lists:flatten([base_url(), "api/increase/", Id]),
    {ok, 200, _Headers, Ref} = hackney:request(get, Uri, [], <<>>, [{pool, test_pool}]),
    {ok, _Body} = hackney:body(Ref).

decreaseValue(Id)->
    Uri = lists:flatten([base_url(), "api/decrease/", Id]),
    {ok, 200, _Headers, Ref} = hackney:request(get, Uri, [], <<>>, [{pool, test_pool}]),
    {ok, _Body} = hackney:body(Ref).

getValue(Id)->
    UriGet = lists:flatten([base_url(), "api/get/", Id]),
    {ok, 200, _, Ref} = hackney:request(get, UriGet, [], <<>>, [{pool, test_pool}]),
    {ok, Resp} = hackney:body(Ref),
    #{<<"code">> := 200, <<"value">> := Val} = jiffy:decode(Resp, [return_maps]),
    Val.

resetValue(Id)->
    Uri = lists:flatten([base_url(), "api/reset/", Id]),
    {ok, 200, _Headers, Ref} = hackney:request(get, Uri, [], <<>>, [{pool, test_pool}]),
    {ok, _Body} = hackney:body(Ref).
