-module(index_handler).

-export([init/2]).
-export([terminate/3]).

%% includes

init(Req, State) ->
    Body = <<"The little engine that could.">>,
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req),
    metrix:increase_counter("web.requests.http.index.200"),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) -> ok.
