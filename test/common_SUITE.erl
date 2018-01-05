-module(common_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        {group, common_app_checks}
    ].

groups() ->
    % Tests = test_tools:tests_in_suite(?MODULE),
    [
        {common_app_checks,
            [parallel, shuffle],
                [app_module_load, sup_module_load]}
    ].


%% =============================================================================
%% init
%% =============================================================================
init_per_group(_Group, Config) ->
    ok = application:load(acounter),
    {ok, _} = application:ensure_all_started(acounter, temporary),
    [{init, true} | Config].


%% =============================================================================
%% end
%% =============================================================================
end_per_group(_Group, _Config) ->
    ok = application:stop(acounter),
    ok = application:unload(acounter),
    ok.


%% =============================================================================
%% group: common_app_checks
%% =============================================================================
app_module_load(_)->
    {module,acounter_app} = code:load_file(acounter_app).

sup_module_load(_)->
    {module,acounter_sup} = code:load_file(acounter_sup).
