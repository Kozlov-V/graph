-module(graph_example).
-export([graph1/0, graph2/0, graph3/0, graph4/0]).
-define(SEC_PER_DAY, 86400).
-define(SEC_PER_HOUR, 3600).
-define(MIL, 1000000).
-define(GD_STYLED, -2).
-define(GD_TRANSPARENT, -6).

graph1() ->
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),

    SizeX = 900,
    ShiftXleft = 85,
    ShiftXright = 30,
    SizeY = 200,
    ShiftY = 36,
    LegendOffsetY = 80,

    Width = SizeX + ShiftXleft + ShiftXright,
    Height = SizeY + ShiftY + LegendOffsetY,

    {ok, G} = gd:new(),
    {ok, Index} = gd:image_create_true_color(G, Width, Height),

    {ok, BackgroundColor} = gd:image_color_allocate(G, Index, 16#F0, 16#F0, 16#F0),
    {ok, GraphBorderColor} = gd:image_color_allocate(G, Index, 16#22, 16#22, 16#22),

    ok = gd:image_filled_rectangle(G, Index, 0, 0, Width, Height, BackgroundColor),
    ok = gd:image_rectangle(G, Index, 0, 0, Width - 1, Height - 1, GraphBorderColor),

    {ok, Binary} = gd:image_png_ptr(G, Index),
    gd:stop(G),
    file:write_file("/tmp/a.png", Binary).

graph2() ->
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),

    SizeX = 900,
    ShiftXleft = 85,
    ShiftXright = 30,
    SizeY = 200,
    ShiftY = 36,
    LegendOffsetY = 80,
    Header = "mylogin : simple graph",
    Font = gd_font:set_font_path(gd_font:new(), "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf"),

    Width = SizeX + ShiftXleft + ShiftXright,
    Height = SizeY + ShiftY + LegendOffsetY,

    {ok, G} = gd:new(),
    {ok, Index} = gd:image_create_true_color(G, Width, Height),

    {ok, BackgroundColor} = gd:image_color_allocate(G, Index, 16#F0, 16#F0, 16#F0),
    {ok, GraphBorderColor} = gd:image_color_allocate(G, Index, 16#22, 16#22, 16#22),
    {ok, TextColor} = gd:image_color_allocate(G, Index, 16#20, 16#20, 16#20),

    ok = gd:image_filled_rectangle(G, Index, 0, 0, Width, Height, BackgroundColor),
    ok = gd:image_rectangle(G, Index, 0, 0, Width - 1, Height - 1, GraphBorderColor),

    PossibleFontSizes = lists:seq(8, 11),
    L1 = [ {S, gd:text_size(G, gd_font:set_point_size(Font, S), Header)} || S <- PossibleFontSizes ],
    {FontSize, {ok, W, _}} = lists:last(lists:takewhile(fun({_, {ok, W, _}}) -> W =< Width end, L1)),
    Xheader = trunc((Width - W) / 2),
    Yheader = 24,
    
    gd:image_string_ft(G, Index, TextColor, gd_font:set_point_size(Font, FontSize), 0, Xheader, Yheader, Header),

    {ok, Binary} = gd:image_png_ptr(G, Index),
    gd:stop(G),
    file:write_file("/tmp/b.png", Binary).

graph3() ->
    Fun = fun(Gd, D) ->
        Width = get_width(D),
        Height = get_height(D),

        Color = palette(Gd),
        draw_rectangle(Gd, Width, Height, Color(background), Color(graphborder)),
        draw_header(Gd, D(fontpath), D(header), Color(text), Width),
        draw_work_period(Gd, D(shiftXleft) + 1, D(shiftY), D(sizeX) + D(shiftXleft) - 1, D(sizeY) + D(shiftY), Color(graph))
    end,
    graph(Fun, "/tmp/c.png").

graph4() ->
    Fun = fun(Gd, D) ->
        Width = get_width(D),
        Height = get_height(D),

        Color = palette(Gd),
        draw_rectangle(Gd, Width, Height, Color(background), Color(graphborder)),
        draw_header(Gd, D(fontpath), D(header), Color(text), Width),
        draw_work_period(Gd, D(shiftXleft) + 1, D(shiftY), D(sizeX) + D(shiftXleft) - 1, D(sizeY) + D(shiftY), Color(graph)),
        draw_time_grid(Gd, D, D(gridPixels), 18537491, 1296, 1376689633, Color(highlight), Color(maingrid))
    end,
    graph(Fun, "/tmp/d.png").

graph(Fun, FilePath) ->
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),
    D = default(),

    Width = get_width(D),
    Height = get_height(D),

    {ok, G} = gd:new(),
    {ok, Index} = gd:image_create_true_color(G, Width, Height),
    Gd = {G, Index},

    Fun(Gd, D),
    {ok, Binary} = gd:image_png_ptr(G, Index),
    gd:stop(G),
    file:write_file(FilePath, Binary).

default() ->
    L = [
        {sizeX, 900},
        {shiftXleft, 85},
        {shiftXright, 30},
        {sizeY, 200},
        {shiftY, 36},
        {legendOffsetY, 80},
        {header, "mylogin : simple graph"},
        {gridPixels, 25},
        {period, 3600},
        {fontpath, "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf"}
    ],
    fun(P) -> proplists:get_value(P, L) end.

get_width(D) when is_function(D) ->
    D(sizeX) + D(shiftXleft) + D(shiftXright).

get_height(D) when is_function(D) ->
    D(sizeY) + D(shiftY) + D(legendOffsetY).

palette({Gd, Index}) ->
    Colors = [
        {background, {16#F0, 16#F0, 16#F0}},
        {graphborder, {16#22, 16#22, 16#22}},
        {text, {16#20, 16#20, 16#20}},
        {graph, {16#FF, 16#FF, 16#FF}},
        {highlight, {16#AA, 16#44, 16#44}},
        {maingrid, {16#AA, 16#AA, 16#AA}}
    ],
    L = [ begin {ok, C} = gd:image_color_allocate(Gd, Index, R, G, B), {T, C} end || {T, {R, G, B}} <- Colors],
    fun(C) -> proplists:get_value(C, L) end.

draw_rectangle({Gd, Index}, Width, Height, BackGroundColor, GraphBorderColor) ->
    ok = gd:image_filled_rectangle(Gd, Index, 0, 0, Width, Height, BackGroundColor),
    ok = gd:image_rectangle(Gd, Index, 0, 0, Width - 1, Height - 1, GraphBorderColor).

draw_work_period({Gd, Index}, Left, Top, Right, Bottom, GraphColor) ->
    gd:image_filled_rectangle(Gd, Index, Left, Top, Right, Bottom, GraphColor).

draw_header({Gd, Index}, FontPath, Text, TextColor, Width) ->
    PossibleFonts = [ gd_font:factory(FontPath, FontSize) || FontSize <- lists:seq(8, 11) ],
    Font = lists:last(lists:takewhile(fun(F) -> {ok, W} = gd:text_width(Gd, F, Text), W =< Width end, PossibleFonts)),
    {ok, TextWidth} = gd:text_width(Gd, Font, Text),
    Xheader = trunc((Width - TextWidth) / 2),
    Yheader = 24,

    gd:image_string_ft(Gd, Index, TextColor, Font, 0, Xheader, Yheader, Text).

-record(tg, {interval, intervalX, offset, offsetX, start, vline}).
draw_time_grid({Gd, Index}, D, GridPixels, Period, SizeX, From, HighlightColor, MainGridColor) ->
    % io:format("From: ~p~n", [From]),
    FontPath = D(fontpath),
    Intervals = [
        {3600, 60},             % 1 min
        {3600, 120},            % 2 min
        {3600, 300},            % 5 min
        {3600, 900},            % 15 min
        {3600, 1800},           % 30 min
        {86400, 3600},          % 1 hour
        {86400, 10800},         % 3 hours
        {86400, 21600},         % 6 hours
        {86400, 43200},         % 12 hours
        {604800, 86400},        % 1 day
        {1209600, 604800},      % 1 week
        {2419200, 1209600},     % 2 weeks
        {4838400, 2419200},     % 4 weeks
        {9676800, 4838400},     % 8 weeks
        {19353600, 9676800}     % 16 weeks
    ],
    Main = fun(E) -> element(1, E) end,
    Sub = fun(E) -> element(2, E) end,
    
    Raw = GridPixels * Period / SizeX,
    % io:format("raw: ~p; '~p' '~p' '~p'~n", [Raw, GridPixels, Period, SizeX]),
    [Interval|_] = lists:usort(fun(E1, E2) -> abs(Sub(E1) - Raw) < abs(Sub(E2) - Raw) end, Intervals),
    
    OffsetMinX = 12,
    TimeGrid = fun(Int) ->
        IntervalX = Int * SizeX / Period,
        Offset = calculate_offset(From, Int),
        OffsetX = Offset * SizeX / Period,
        Start = case OffsetX < OffsetMinX of true -> 1; false -> 0 end,
        VlineCountMax = trunc((Period - Offset) / Int),
        VlineCount = lists:last(lists:takewhile(fun(C) -> SizeX - OffsetX - C*IntervalX >= OffsetMinX end, lists:seq(0, VlineCountMax))),
        #tg{ interval=Int, intervalX=IntervalX, offset=Offset, offsetX=OffsetX, start=Start, vline=VlineCount }
    end,

    SubTimeGrid = TimeGrid(Sub(Interval)),
    MainTimeGrid = TimeGrid(Main(Interval)),
    % io:format("interval: ~p~n", [Interval]),
    % io:format("sub: ~p~n", [SubTimeGrid]),
    % io:format("main: ~p~n", [MainTimeGrid]),
    
    F = fun(N) ->
        T = From + N * SubTimeGrid#tg.interval + SubTimeGrid#tg.offset,
        NewPos = N * SubTimeGrid#tg.intervalX + SubTimeGrid#tg.offsetX,
        New = T + timezone(unixtime_to_erlangtime(From)) - timezone(unixtime_to_erlangtime(T)),   % daylight saving
        NewTime = {NewDate, {H, M, _}} = calendar:now_to_local_time(unixtime_to_erlangtime(New)),
        DayOfWeek = calendar:day_of_the_week(NewDate),

        if
            SubTimeGrid#tg.interval < ?SEC_PER_HOUR andalso M == 0 ->
                draw_main_period({Gd, Index}, FontPath, NewTime, NewPos, D(shiftXleft), D(sizeY), D(shiftY), HighlightColor, MainGridColor);
            SubTimeGrid#tg.interval >= ?SEC_PER_HOUR andalso SubTimeGrid#tg.interval < ?SEC_PER_DAY andalso H == 0 ->
                draw_main_period({Gd, Index}, FontPath, NewTime, NewPos, D(shiftXleft), D(sizeY), D(shiftY), HighlightColor, MainGridColor);
            SubTimeGrid#tg.interval == ?SEC_PER_DAY andalso DayOfWeek == 1 -> 
                draw_main_period({Gd, Index}, FontPath, NewTime, NewPos, D(shiftXleft), D(sizeY), D(shiftY), HighlightColor, MainGridColor);
            SubTimeGrid#tg.interval > ?SEC_PER_DAY andalso 
            ((N * SubTimeGrid#tg.interval) rem MainTimeGrid#tg.interval) + SubTimeGrid#tg.offset == MainTimeGrid#tg.offset ->
                draw_main_period({Gd, Index}, FontPath, NewTime, NewPos, D(shiftXleft), D(sizeY), D(shiftY), HighlightColor, MainGridColor);
            true ->
                true
        end
    end,
    
    [ F(N) || N <- lists:seq(SubTimeGrid#tg.start, SubTimeGrid#tg.vline) ].

unixtime_to_erlangtime(Time) -> 
    {Time div ?MIL, Time rem ?MIL, 0}.

calculate_offset(FromU, Interval) ->
    From = unixtime_to_erlangtime(FromU),
    case Interval > ?SEC_PER_DAY of
        true ->
            {FromDate, FromTime} = calendar:now_to_local_time(From),
            Next = FromU + (7 - calendar:day_of_the_week(FromDate) rem 7) * ?SEC_PER_DAY,
            % io:format("fromdate: ~p; offset: ~p~n", [FromDate, (7 - calendar:day_of_the_week(FromDate) rem 7) * ?SEC_PER_DAY]),
            {NextDate, _} = calendar:now_to_local_time(unixtime_to_erlangtime(Next)),
            % io:format("nextdate = ~p~n", [NextDate]),
            calendar:datetime_to_gregorian_seconds({NextDate, {0,0,0}}) - calendar:datetime_to_gregorian_seconds({FromDate, FromTime});
        false ->
            TZ = timezone(From),
            Interval - (FromU + TZ) rem Interval
    end.

timezone(Time) -> 
    LocalTime = calendar:now_to_local_time(Time),
    UTCTime = calendar:now_to_universal_time(Time),
    calendar:datetime_to_gregorian_seconds(LocalTime) - calendar:datetime_to_gregorian_seconds(UTCTime).

draw_main_period({Gd, Index}, FontPath, {{_Year, Month, Day}, {Hour, Min, _Sec}}, Pos, ShiftXleft, SizeY, ShiftY, HighlightColor, MainGridColor) ->
    OffsetX = ShiftXleft,
    OffsetY = SizeY + ShiftY,
    DateArgs = case {Hour, Min} of
        {0, 0} ->
            ["~2..0B.~2..0B", [Day, Month]];
        {0, _} ->
            ["~2..0B.~2..0B ~2..0B:~2..0B", [Day, Month, Hour, Min]];
        {_, _} ->
            ["~2..0B:~2..0B", [Hour, Min]]
    end,
    Date = lists:flatten(apply(io_lib, format, DateArgs)),
    Font = gd_font:factory(FontPath, 8),
    {ok, W, H} = text_size({Gd, Index}, Font, Date, 3.14/2),
    % io:format("x = '~p'; y = '~p'; pos = '~p'; date = '~s'~n", [trunc(OffsetX + Pos + W / 2), trunc(OffsetY + W + 6), Pos, Date]),
    % io:format("OffsetY = '~p'; height = '~p';~n", [OffsetY, H]),
    gd:image_string_ft(Gd, Index, HighlightColor, Font, 3.14/2, trunc(OffsetX + Pos + W / 2), trunc(OffsetY + H + 6), Date),
    image_dashed_line(Gd, Index, trunc(ShiftXleft + Pos), trunc(ShiftY), trunc(ShiftXleft + Pos), trunc(SizeY + ShiftY), MainGridColor).

text_size({GD, Index}, Font, String, Angle) ->
    { ok, Bounds } = gd:image_string_ft (GD, Index, 0, Font, Angle, 0, 0, String),
    Width = gd_bounds:max_x(Bounds) - gd_bounds:min_x(Bounds),
    Height = gd_bounds:max_y(Bounds) - gd_bounds:min_y(Bounds),
    { ok, Width, Height }.

image_dashed_line(Gd, Index, X1, Y1, X2, Y2, Color) ->
    gd:image_set_style(Gd, Index, [Color, Color, ?GD_TRANSPARENT, ?GD_TRANSPARENT]),
    gd:image_line(Gd, Index, X1, Y1, X2, Y2, ?GD_STYLED).
