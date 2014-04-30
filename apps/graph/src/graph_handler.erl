-module(graph_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {ContentEncoding, Req2} = cowboy_req:header(<<"content-encoding">>, Req2),
    Content = if 
        ContentEncoding == <<"gzip">> ->
            zlib:gunzip(Body);
        true ->
            Body
    end,
    {Decoded} = jiffy:decode(Content),
    From = proplists:get_value(<<"from">>, Decoded),
    Period = proplists:get_value(<<"period">>, Decoded),
    Title = proplists:get_value(<<"title">>, Decoded, <<>>),
    Width = proplists:get_value(<<"width">>, Decoded, 900),
    Height = proplists:get_value(<<"height">>, Decoded, 200),
    Data = [ E || {E} <- proplists:get_value(<<"data">>, Decoded) ],
    Bin = graph:graph(graph:dim(Width, Height), graph:default_theme(), From, Period, Title, Data),
    {ok, Req3} = cowboy_req:reply(200, [{<<"content-type">>, <<"image/png">>}], Bin, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
