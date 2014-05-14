-module(graph_app).

-behaviour(application).

%% Application callbacks
-export([start/0]).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    application:start(graph).

start(_StartType, _StartArgs) ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(alog),
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),

    Vroutes = [
        {<<"/graph/[...]">>, graph_handler, []}
    ],
    Routes = [{'_', Vroutes}],
    Dispatch = cowboy_router:compile(Routes),
    {ok, Port} = application:get_env(graph, port),
    cowboy:start_http(my_http_listener, 100, 
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]),
    graph_sup:start_link().

stop(_State) ->
    ok.
