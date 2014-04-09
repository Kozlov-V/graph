-module(graph).
-compile(export_all).

-define(SEC_PER_HOUR, 3600).
-define(SEC_PER_DAY, 86400).
-define(FONT_SIZES, [8,9,10,11]).
-define(A90, 3.14/2).
-define(GD_STYLED, -2).
-define(GD_TRANSPARENT, -6).
-define(WIDTH, (Dim(shiftXleft) + Dim(sizeX) + Dim(shiftXright))).
-define(HEIGHT, (Dim(shiftY) + Dim(sizeY) + Dim(legendOffsetY))).

default_dim() ->
    List = [
        {sizeX, 1296},
        {shiftXleft, 85},
        {shiftXright, 30},
        {sizeY, 200},
        {shiftY, 36},
        {gridPixels, 25},
        {legendOffsetY, 90}],
    fun(P) -> proplists:get_value(P, List) end.

default_theme() ->
    [
        {background, {16#33, 16#33, 16#33}},
        {graphborder, {16#88, 16#88, 16#88}},
        {graph, {16#0A, 16#0A, 16#0A}},
        {gridborder, {16#EF, 16#EF, 16#EF}},
        {grid, {16#22, 16#22, 16#22}},
        {highlight, {16#FF, 16#55, 16#00}},
        {maingrid, {16#4F, 16#4F, 16#4F}},
        {text, {16#DF, 16#DF, 16#DF}},
        {white, {16#FF, 16#FF, 16#FF}}
    ].

get_palette({Gd, Index}, Colors) ->
    L = [ begin {ok, C} = gd:image_color_allocate(Gd, Index, R, G, B), {T, C} end || {T, {R, G, B}} <- Colors],
    fun(C) -> proplists:get_value(C, L) end.

graph(Dim, Theme, From, Period) ->
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),

    {ok, Gd} = gd:new(),
    {ok, Index} = gd:image_create_true_color(Gd, ?WIDTH, ?HEIGHT),
    G = {Gd, Index},

    PossibleThemes = [blue, black_blue, dark_orange],
    T = case lists:member(Theme, PossibleThemes) of true -> ?MODULE:Theme(); false -> default_theme() end,
    Palette = get_palette(G, T),

    Fontpath = "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf",

    draw_rectangle(G, Dim, Palette),
    draw_header(G, Dim, Palette, Fontpath, "mylogin : simple graph"),
    draw_work_period(G, Dim, Palette),
    draw_time_grid(G, Dim, Palette, Fontpath, From, Period),
    draw_x_axis(G, Dim, Palette),
    draw_y_axis(G, Dim, Palette),

    {ok, Binary} = gd:image_png_ptr(Gd, Index),
    gd:stop(Gd),
    Binary.

draw_rectangle({Gd, Index}, Dim, Palette) ->
    ok = gd:image_filled_rectangle(Gd, Index, 0, 0, ?WIDTH, ?HEIGHT, Palette(background)),
    ok = gd:image_rectangle(Gd, Index, 0, 0, ?WIDTH - 1, ?HEIGHT - 1, Palette(graphborder)).

draw_header({Gd, Index}, Dim, Palette, FontPath, Text) ->
    PossibleFonts = [ gd_font:factory(FontPath, FontSize) || FontSize <- ?FONT_SIZES ],
    Font = lists:last(lists:takewhile(fun(F) -> {ok, W} = gd:text_width(Gd, F, Text), W =< Dim(width) end, PossibleFonts)),
    {ok, TextWidth} = gd:text_width(Gd, Font, Text),
    Xheader = trunc((?WIDTH - TextWidth) / 2),
    Yheader = 24,
    {ok, _} = gd:image_string_ft(Gd, Index, Palette(text), Font, 0, Xheader, Yheader, Text).

draw_work_period({Gd, Index}, Dim, Palette) ->
    ok = gd:image_filled_rectangle(Gd, Index, Dim(shiftXleft) + 1, Dim(shiftY), Dim(sizeX) + Dim(shiftXleft) - 1, Dim(sizeY) + Dim(shiftY), Palette(graph)).

mapX(Dim, From, Period) ->
    fun(T) ->
        trunc(Dim(shiftXleft) + (T - From) * Dim(sizeX) / Period)
    end.

mapY(Dim, Y0, Ytop) ->
    fun(Y) ->
        trunc(Dim(sizeY) + Dim(shiftY) - (Y - Y0) * Dim(sizeY) / (Ytop - Y0))
    end.

draw_time_grid({Gd, Index}, Dim, Palette, FontPath, From, Period) ->
    {ok, List} = calc_time_grid(From, Period, Dim(gridPixels) / Dim(sizeX)),
    MapX = mapX(Dim, From, Period),
    Ybot = Dim(sizeY) + Dim(shiftY),
    Ytop = Dim(shiftY),

    DrawDate = fun(Timestamp, Date, FontSize, Color) ->
        Font = gd_font:factory(FontPath, FontSize),
        {ok, W, H} = gd:text_size(Gd, Font, Date, ?A90),
        X = MapX(Timestamp),
        Y = trunc(Ybot + H + 6),
        gd:image_string_ft(Gd, Index, Color, Font, ?A90, trunc(X + W/2), Y, Date),
        X
    end,
    [ if
        Type == "start" orelse Type == "end" -> 
            DrawDate(Timestamp, Date, 8, Palette(highlight));
        Type == "main" ->
            X = DrawDate(Timestamp, Date, 8, Palette(highlight)),
            image_dashed_line(Gd, Index, X, Ytop, X, Ybot, Palette(maingrid));
        Type == "sub" ->
            X = DrawDate(Timestamp, Date, 7, Palette(text)),
            image_dashed_line(Gd, Index, X, Ytop, X, Ybot, Palette(grid))
      end || {Type, Timestamp, Date} <- List ].

draw_x_axis({Gd, Index}, Dim, Palette) ->
    Xleft = trunc(Dim(shiftXleft) - 4),
    Xright = trunc(Dim(shiftXleft) + Dim(sizeX) + 5),
    Y = Dim(sizeY) + Dim(shiftY) + 1,
    gd:image_line(Gd, Index, Xleft, Y, Xright, Y, Palette(gridborder)),
    gd:image_filled_polygon(Gd, Index, [{Xright, Y-3}, {Xright, Y+3}, {Xright+5, Y}], Palette(white)),
    gd:image_line(Gd, Index, Xright, Y-3, Xright, Y+3, Palette(gridborder)),
    gd:image_line(Gd, Index, Xright, Y+3, Xright+5, Y, Palette(gridborder)),
    gd:image_line(Gd, Index, Xright+5, Y, Xright, Y-3, Palette(gridborder)).

draw_y_axis({Gd, Index}, Dim, Palette) ->
    X = trunc(Dim(shiftXleft) - 1),
    Ytop = trunc(Dim(shiftY) - 5),
    Ybot = trunc(Dim(shiftY) + Dim(sizeY) + 4),
    gd:image_line(Gd, Index, X, Ytop, X, Ybot, Palette(gridborder)),
    gd:image_filled_polygon(Gd, Index, [{X-3, Ytop}, {X+3, Ytop}, {X, Ytop-5}], Palette(white)),
    gd:image_line(Gd, Index, X-3, Ytop, X+3, Ytop, Palette(gridborder)),
    gd:image_line(Gd, Index, X-3, Ytop, X, Ytop-5, Palette(gridborder)),
    gd:image_line(Gd, Index, X+3, Ytop, X, Ytop-5, Palette(gridborder)).


-spec calc_time_grid(From :: non_neg_integer(), Period :: non_neg_integer(), GridCoef :: float()) ->
    {'ok', [{atom(), non_neg_integer(), string()}]} | {'error', string()}.

% GridCoef = (desired grid cell width) / (chart width), i.e., 25px / 900px
calc_time_grid(From, Period, GridCoef) when is_integer(From), is_integer(Period) ->
    Raw = Period * GridCoef,
    Intervals = [
        {?SEC_PER_HOUR, 60},                        % 1 min
        {?SEC_PER_HOUR, 120},                       % 2 min
        {?SEC_PER_HOUR, 300},                       % 5 min
        {?SEC_PER_HOUR, 900},                       % 15 min
        {?SEC_PER_HOUR, 1800},                      % 30 min
        {?SEC_PER_DAY, ?SEC_PER_HOUR},              % 1 hour
        {?SEC_PER_DAY, 3*?SEC_PER_HOUR},            % 3 hours
        {?SEC_PER_DAY, 6*?SEC_PER_HOUR},            % 6 hours
        {?SEC_PER_DAY, 12*?SEC_PER_HOUR},           % 12 hours
        {7*?SEC_PER_DAY, ?SEC_PER_DAY},             % 1 day
        {2*7*?SEC_PER_DAY, 7*?SEC_PER_DAY},         % 1 week
        {4*7*?SEC_PER_DAY, 2*7*?SEC_PER_DAY},       % 2 weeks
        {8*7*?SEC_PER_DAY, 4*7*?SEC_PER_DAY},       % 4 weeks
        {16*7*?SEC_PER_DAY, 8*7*?SEC_PER_DAY},      % 8 weeks
        {32*7*?SEC_PER_DAY, 16*7*?SEC_PER_DAY}      % 16 weeks
    ],
    [{MainInterval,Interval}|_] = lists:usort(fun({_,A}, {_,B}) -> abs(A-Raw) < abs(B-Raw) end, Intervals),
    Offset = calc_offset(From, Interval),
    MainOffset = calc_offset(From, MainInterval),
    Start = case (Offset/Period) < 0.48*GridCoef of true -> 1; false -> 0 end,
    VlineMax = trunc((Period*(1 - 0.48*GridCoef) - Offset) / Interval),
    F = fun(N) ->
        T1 = From + N*Interval + Offset,
        T2 = T1 + timezone(unixtime_to_erlangtime(From)) - timezone(unixtime_to_erlangtime(T1)),    % daylight saving
        {{Year, Month, Day}, {Hour, Min, _}} = calendar:now_to_local_time(unixtime_to_erlangtime(T2)),
        DayOfWeek = calendar:day_of_the_week({Year, Month, Day}),

        if 
            Interval < ?SEC_PER_HOUR andalso Hour == 0 andalso Min == 0 ->
                {"main",T2, date2str("~2..0B.~2..0B", [Day, Month])};
            Interval < ?SEC_PER_HOUR andalso Min == 0 ->
                {"main",T2, date2str("~2..0B:~2..0B", [Hour, Min])};
            Interval >= ?SEC_PER_HOUR andalso Interval < ?SEC_PER_DAY andalso Hour == 0 andalso Min == 0 ->
                {"main", T2, date2str("~2..0B.~2..0B", [Day, Month])};
            Interval >= ?SEC_PER_HOUR andalso Interval < ?SEC_PER_DAY andalso Hour == 0 ->
                {"main", T2, date2str("~2..0B.~2..0B ~2..0B:~2..0B", [Day, Month, Hour, Min])};
            Interval == ?SEC_PER_DAY andalso DayOfWeek == 7 ->   % sunday
                {"main", T2, date2str("~2..0B.~2..0B", [Day, Month])};
            Interval > ?SEC_PER_DAY andalso ((N*Interval) rem  MainInterval) + Offset == MainOffset ->
                {"main", T2, date2str("~2..0B.~2..0B", [Day, Month])};
            Interval >= ?SEC_PER_DAY ->
                {"sub", T2, date2str("~2..0B.~2..0B", [Day, Month])};
            Interval < ?SEC_PER_DAY -> 
                {"sub", T2, date2str("~2..0B:~2..0B", [Hour, Min])}
        end

    end,
    Res = [ F(N) || N <- lists:seq(Start, VlineMax) ],
    {{_, Sm, Sd}, {Sh, Smin, _}} = calendar:now_to_local_time(unixtime_to_erlangtime(From)),
    {{_, Em, Ed}, {Eh, Emin, _}} = calendar:now_to_local_time(unixtime_to_erlangtime(From + Period)),
    
    S = {"start", From, date2str("~2..0B.~2..0B ~2..0B:~2..0B", [Sd, Sm, Sh, Smin])},
    E = {"end", From + Period, date2str("~2..0B.~2..0B ~2..0B:~2..0B", [Ed, Em, Eh, Emin])},
    {ok, [S] ++ Res ++ [E]}.

calc_offset(FromU, Interval) ->
    From = unixtime_to_erlangtime(FromU),
    case Interval > ?SEC_PER_DAY of
        true ->
            {FromDate, FromTime} = calendar:now_to_local_time(From),
            Next = FromU + (7 - calendar:day_of_the_week(FromDate) rem 7) * ?SEC_PER_DAY,
            {NextDate, _} = calendar:now_to_local_time(unixtime_to_erlangtime(Next)),
            calendar:datetime_to_gregorian_seconds({NextDate, {0,0,0}}) - calendar:datetime_to_gregorian_seconds({FromDate, FromTime});
        false ->
            TZ = timezone(From),
            Interval - (FromU + TZ) rem Interval
    end.

convert_units(Unixtime, unixtime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(unixtime_to_erlangtime(Unixtime)),
    sprintf("~4..0B.~2..0B.~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Min, Sec]).

convert_units(Value, Pow, Units, Length) ->
    IsBinary = lists:member(Units, ["B", "Bps"]),
    IsBlackListed = lists:member(Units, ["%", "ms", "rpm", "RPM"]),
    IsEmptyUnits = Units == "",
    Step = case IsBinary of true -> 1024; false -> 1000 end,
    V4 = round(Value, 4),
    A4 = abs(V4),
    if
        IsBlackListed orelse IsEmptyUnits ->
            V = case A4 >= 0.01 of true -> round(Value, 2); false -> Value end,
            strip_trailing_zeros(sprintf("~.6..f", [V])) ++ " " ++ Units;
        A4 == 0  ->
            case is_integer(Length) of 
                true ->
                    sprintf("~." ++ integer_to_list(Length) ++ "..f ~s", [V4, Units]);
                false ->
                    sprintf("~p ~s", [0, Units])
            end;
        A4 < 1 ->
            case is_integer(Length) of 
                true -> 
                    sprintf("~." ++ integer_to_list(Length) ++ "..f ~s", [V4, Units]);
                false ->
                    sprintf("~p ~s", [V4, Units])
            end;
        true ->
            P = case is_integer(Pow) of true -> Pow; false -> ceiling(math:log(A4) / math:log(Step)) end,
            V = Value / math:pow(Step, P),
            V10 = strip_trailing_zeros(sprintf("~.10..f", [V])),
            case is_integer(Length) of
                true ->
                    sprintf("~." ++ integer_to_list(Length) ++ "..f ~s~s", [V, pow_to_prefix(P), Units]);
                false ->
                    sprintf("~s ~s~s", [V10, pow_to_prefix(P), Units])
            end
    end.

%% 
timezone(Time) ->
    LocalTime = calendar:now_to_local_time(Time),
    UTCTime = calendar:now_to_universal_time(Time),
    calendar:datetime_to_gregorian_seconds(LocalTime) - calendar:datetime_to_gregorian_seconds(UTCTime).

unixtime_to_erlangtime(Timestamp) ->
    {Timestamp div 1000000, Timestamp rem 1000000, 0}.

-spec date2str(Format :: string(), Args :: [pos_integer()]) -> 
    string().

date2str(Format, Args) when is_list(Format), is_list(Args) ->
    lists:flatten(io_lib:format(Format, Args)).

pow_to_prefix(Pow) ->
    case Pow of
        0 -> "";
        1 -> "K";
        2 -> "M";
        3 -> "G";
        4 -> "T";
        5 -> "P";
        6 -> "E";
        7 -> "Z";
        8 -> "Y"
    end.

sprintf(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

strip_trailing_zeros(String) ->
    string:strip(string:strip(String, right, $0), right, $.).

round(Value, Limit) when is_integer(Limit), Limit >= 0 ->
    M = math:pow(10, Limit),
    round(Value * M) / M.

ceiling(X) ->
    T = trunc(X),
    case X - T of 
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

image_dashed_line(Gd, Index, X1, Y1, X2, Y2, Color) ->
    gd:image_set_style(Gd, Index, [Color, Color, ?GD_TRANSPARENT, ?GD_TRANSPARENT]),
    gd:image_line(Gd, Index, trunc(X1), trunc(Y1), trunc(X2), trunc(Y2), ?GD_STYLED).

