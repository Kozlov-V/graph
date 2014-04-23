-module(graph_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], <<"dummy">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
