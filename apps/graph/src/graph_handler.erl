-module(graph_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Now = now(),
    try input_to_output(Req) of

        Bin when is_binary(Bin) ->
            {ok, Req3} = cowboy_req:reply(200, [{<<"content-type">>, <<"image/png">>}], Bin, Req),
            {ok, Req3, State}
    catch
        Exception:Reason ->
            {_,_,Ms} = Now,
            {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
            Filename = string:join([ integer_to_list(E) || E <- [Year, Month, Day, Hour, Min, Sec, Ms] ], "-"),
            file:write_file("/tmp/" ++ Filename ++ ".log", io_lib:fwrite("~p.", [{Exception, Reason, Req}])),
            {ok, Req3} = cowboy_req:reply(500, [], <<>>, Req),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

input_to_output(Req) ->
    {Mega, Sec, _} = now(),
    Now = Mega*1000000 + Sec,

    {ok, Body, Req2} = cowboy_req:body(Req),
    {ContentEncoding, Req2} = cowboy_req:header(<<"content-encoding">>, Req2),
    Content = if 
        ContentEncoding == <<"gzip">> ->
            zlib:gunzip(Body);
        true ->
            Body
    end,
    {Decoded} = jiffy:decode(Content),

    Period = proplists:get_value(<<"period">>, Decoded, 3600),
    From = proplists:get_value(<<"from">>, Decoded, Now - Period),
    Title = proplists:get_value(<<"title">>, Decoded, <<>>),
    Width = proplists:get_value(<<"width">>, Decoded, 900),
    Height = proplists:get_value(<<"height">>, Decoded, 200),
    Data = [ E || {E} <- proplists:get_value(<<"data">>, Decoded) ],
    ThemeB = proplists:get_value(<<"theme">>, Decoded, <<>>),
    Theme = list_to_atom(binary_to_list(ThemeB)),
    graph:graph(graph:dim(Width, Height), Theme, From, Period, Title, Data).
    
