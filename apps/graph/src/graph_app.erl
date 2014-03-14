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
    graph_sup:start_link().

stop(_State) ->
    ok.
