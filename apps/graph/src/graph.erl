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
        {gridPixelsVert, 40},
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
        {white, {16#FF, 16#FF, 16#FF}},
        {green, {16#00, 16#FF, 16#00}}
    ].

get_palette({Gd, Index}, Colors) ->
    L = [ begin {ok, C} = gd:image_color_allocate(Gd, Index, R, G, B), {T, C} end || {T, {R, G, B}} <- Colors],
    fun(C) -> proplists:get_value(C, L) end.

graph(Dim, Theme, From, Period, Data) ->
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),

    {ok, Gd} = gd:new(),
    {ok, Index} = gd:image_create_true_color(Gd, ?WIDTH, ?HEIGHT),
    G = {Gd, Index},

    PossibleThemes = [blue, black_blue, dark_orange],
    T = case lists:member(Theme, PossibleThemes) of true -> ?MODULE:Theme(); false -> default_theme() end,
    Palette = get_palette(G, T),

    % Fontpath = "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf",
    {ok, Cwd} = file:get_cwd(),
    Fontpath = Cwd ++ "/DejaVuSans.ttf",

    draw_rectangle(G, Dim, Palette),
    draw_header(G, Dim, Palette, Fontpath, "mylogin : simple graph"),
    draw_work_period(G, Dim, Palette),
    draw_time_grid(G, Dim, Palette, Fontpath, From, Period),
    draw_x_axis(G, Dim, Palette),
    draw_y_axis(G, Dim, Palette),

    AllData = lists:flatten([ proplists:get_value(data, L) || L <- Data ]),
    MinY = lists:min([ Y || {_,Y} <- AllData ]),
    MaxY = lists:max([ Y || {_,Y} <- AllData ]),
    Units = [ proplists:get_value(units, L, "") || L <- Data ],
    U = hd(Units),

    IsBinary = lists:member(U, ["B", "Bps"]),
    Type = case IsBinary of true -> binary; false -> decimal end,
    {Min, Max, Interval} = draw_horizontal_grid(G, Dim, Palette, MinY, MaxY, Type),
    draw_sides(G, Dim, Palette, Fontpath, Min, Max, Interval, U, Type),
    draw_chart(G, Dim, Palette, From, Period, Min, Max, proplists:get_value(data, hd(Data)), proplists:get_value(color, hd(Data))),

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
        round(Dim(shiftXleft) + (T - From) * Dim(sizeX) / Period)
    end.

mapY(Dim, Y0, Ytop) ->
    fun(Y) ->
        round(Dim(sizeY) + Dim(shiftY) - (Y - Y0) * Dim(sizeY) / (Ytop - Y0))
    end.

draw_chart({Gd, Index}, Dim, Palette, From, Period, Min, Max, Data, Color) ->
    MapX = mapX(Dim, From, Period),
    MapY = mapY(Dim, Min, Max),
    Lines = lists:zip(lists:sublist(Data, length(Data)-1), tl(Data)),
    [ gd:image_line(Gd, Index, MapX(X1), MapY(Y1), MapX(X2), MapY(Y2), Palette(Color)) || {{X1,Y1},{X2,Y2}} <- Lines ].


draw_time_grid({Gd, Index}, Dim, Palette, FontPath, From, Period) ->
    {ok, List} = calc_time_grid(From, Period, Dim(gridPixels) / Dim(sizeX)),
    MapX = mapX(Dim, From, Period),
    Ybot = trunc(Dim(sizeY) + Dim(shiftY)),
    Ytop = trunc(Dim(shiftY)),

    DrawDate = fun(Timestamp, Date, FontSize, Color) ->
        Font = gd_font:factory(FontPath, FontSize),
        {ok, W, H} = gd:text_size(Gd, Font, Date, ?A90),
        X = MapX(Timestamp),
        Y = Ybot + H + 6,
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

draw_horizontal_grid({Gd,Index}, Dim, Palette, MinY, MaxY, Type) ->
    {Min, Max, Interval} = calc_horizontal_grid(MinY, MaxY, Dim(gridPixelsVert) / Dim(sizeY), Type),

    StepT = Interval * Dim(sizeY) / (Max - Min),
    Step = case (Max - Min) / Interval < round(Dim(sizeY) / Dim(gridPixels)) of true -> StepT / 2; false -> StepT end,
    [ begin
        Y = trunc(Dim(shiftY) + Dim(sizeY) - N*Step),
        image_dashed_line(Gd, Index, Dim(shiftXleft), Y, Dim(shiftXleft) + Dim(sizeX), Y, Palette(maingrid)) 
      end || N <- lists:seq(1, trunc(Dim(sizeY)/Step)) ],
    {Min, Max, Interval}. 

draw_sides({Gd, Index}, Dim, Palette, Fontpath, Min, Max, Interval, Units, Type) ->
    L = if
        Min == 0 andalso Max == 0 -> [1];
        Min == 0 -> [Max];
        Max == 0 -> [Min];
        true -> [Min, Max]
    end,
    Pow = lists:max([ pow_of(1000, V) || V <- L ]),
    MapY = mapY(Dim, Min, Max),
    Steps = lists:seq(0, round((Max - Min)/Interval)),
    ML = calc_max_length_after_dot([ convert_units(Min + N*Interval, "", no_units, Type, Pow, undefined) || N <- Steps ]),
    [ begin 
        Y = Min + N * Interval,
        Str = convert_units(Min + N*Interval, Units, no_units, Type, Pow, ML),
        Font = gd_font:factory(Fontpath, 8),
        {ok, W, _H} = gd:text_size(Gd, Font, Str, 0),
        gd:image_string_ft(Gd, Index, Palette(text), Font, 0, Dim(shiftXleft) - 9 - W, MapY(Y) + 4, Str)
      end || N <- lists:seq(0, round((Max - Min)/Interval)) ].

calc_horizontal_grid(MinY, MaxY, GridCoef) ->
    calc_horizontal_grid(MinY, MaxY, GridCoef, decimal).
    
% GridCoef = (desired grid cell height) / (chart height), i.e., 40px / 900px
calc_horizontal_grid(N2, X2, GridCoef, Type) ->
    {N, X} = if
        N2 == X2 andalso X2 == 0 ->
            {0, 1};
        N2 == X2 ->
            {0, X2};
        true ->
            {N2, X2}
    end,
    {MinY, MaxY} = case abs( (X - N) / X ) =< 0.1 of
        true ->
            {case N > 0 of true -> 0.95*N; false -> 1.05*N end, case X > 0 of true -> 1.05*X; false -> 0.95*X end};
        false ->
            {N, X}
    end,
    Raw = (MaxY - MinY) * GridCoef,
    Intervals = [ math:pow(10, P) * M || P <- lists:seq(-4,18), M <- [1,2,5] ],
    [Int|_] = lists:usort(fun(A,B) -> abs(Raw - A) < abs(Raw - B) end, Intervals),
    Interval = if 
        Type == binary ->
            get_base_1024_interval(Int, MinY, MaxY);
        true ->
            Int
    end,

    MinT = Interval * floor(MinY / Interval),
    MaxT = Interval * ceiling(MaxY / Interval),

    Min = case MinT == MinY andalso MinT /= 0 of true -> MinT - Interval; false -> MinT end,
    Max = case MaxT == MaxY andalso MaxT /= 0 of true -> MaxT + Interval; false -> MaxT end,
    {Min, Max, Interval}.

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
                {"main",T2, sprintf("~2..0B.~2..0B", [Day, Month])};
            Interval < ?SEC_PER_HOUR andalso Min == 0 ->
                {"main",T2, sprintf("~2..0B:~2..0B", [Hour, Min])};
            Interval >= ?SEC_PER_HOUR andalso Interval < ?SEC_PER_DAY andalso Hour == 0 andalso Min == 0 ->
                {"main", T2, sprintf("~2..0B.~2..0B", [Day, Month])};
            Interval >= ?SEC_PER_HOUR andalso Interval < ?SEC_PER_DAY andalso Hour == 0 ->
                {"main", T2, sprintf("~2..0B.~2..0B ~2..0B:~2..0B", [Day, Month, Hour, Min])};
            Interval == ?SEC_PER_DAY andalso DayOfWeek == 7 ->   % sunday
                {"main", T2, sprintf("~2..0B.~2..0B", [Day, Month])};
            Interval > ?SEC_PER_DAY andalso ((N*Interval) rem  MainInterval) + Offset == MainOffset ->
                {"main", T2, sprintf("~2..0B.~2..0B", [Day, Month])};
            Interval >= ?SEC_PER_DAY ->
                {"sub", T2, sprintf("~2..0B.~2..0B", [Day, Month])};
            Interval < ?SEC_PER_DAY -> 
                {"sub", T2, sprintf("~2..0B:~2..0B", [Hour, Min])}
        end

    end,
    Res = [ F(N) || N <- lists:seq(Start, VlineMax) ],
    {{_, Sm, Sd}, {Sh, Smin, _}} = calendar:now_to_local_time(unixtime_to_erlangtime(From)),
    {{_, Em, Ed}, {Eh, Emin, _}} = calendar:now_to_local_time(unixtime_to_erlangtime(From + Period)),
    
    S = {"start", From, sprintf("~2..0B.~2..0B ~2..0B:~2..0B", [Sd, Sm, Sh, Smin])},
    E = {"end", From + Period, sprintf("~2..0B.~2..0B ~2..0B:~2..0B", [Ed, Em, Eh, Emin])},
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

%% 
timezone(Time) ->
    LocalTime = calendar:now_to_local_time(Time),
    UTCTime = calendar:now_to_universal_time(Time),
    calendar:datetime_to_gregorian_seconds(LocalTime) - calendar:datetime_to_gregorian_seconds(UTCTime).

unixtime_to_erlangtime(Timestamp) ->
    {Timestamp div 1000000, Timestamp rem 1000000, 0}.

-spec sprintf(Format :: string(), Args :: list()) -> string().

sprintf(Format, Args) when is_list(Format), is_list(Args) ->
    lists:flatten(io_lib:format(Format, Args)).

strip_trailing_zeros(String) ->
    string:strip(string:strip(String, right, $0), right, $.).

%% drawing functions
-spec image_dashed_line(_, _, X1 :: integer(), Y1 :: integer(), 
    X2 :: integer(), Y2 :: integer(), Color :: integer()) -> ok.

image_dashed_line(Gd, Index, X1, Y1, X2, Y2, Color) ->
    ok = gd:image_set_style(Gd, Index, [Color, Color, ?GD_TRANSPARENT, ?GD_TRANSPARENT]),
    ok = gd:image_line(Gd, Index, X1, Y1, X2, Y2, ?GD_STYLED).

-spec pow_to_prefix(Pow :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8) -> string().

pow_to_prefix(Pow) when is_integer(Pow), Pow >= 0, Pow =< 8 ->
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

%% math functions
-spec round(Value :: number(), Limit :: non_neg_integer()) -> float().

round(Value, Limit) when is_integer(Limit), Limit >= 0 ->
    M = math:pow(10, Limit),
    round(Value * M) / M.

-spec ceiling(X :: number()) -> integer().

ceiling(X) ->
    T = trunc(X),
    case X - T of 
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

-spec floor(X :: number()) -> integer().

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

-spec groupByX([{any(), any()}]) -> [{any(), list()}].

groupByX(List) ->
    G = lists:foldr(fun({X,Y}, Acc) -> 
            E = case gb_trees:lookup(X, Acc) of none -> [Y]; {value, V} -> [Y|V] end, 
            gb_trees:enter(X, E, Acc)
        end, gb_trees:empty(), List),
    gb_trees:to_list(G).

-spec pow_of(Step :: number(), Value :: number()) -> integer().

pow_of(Step, Value) when Step > 1 andalso Value /= 0 ->
    trunc(math:log(abs(Value)) / math:log(Step)).

%% application specified functions
-spec convert_to_base_1024(Value :: number()) -> float().

convert_to_base_1024(Value) ->
    % 204800 (200 KBytes) with '1024' step convert to 209715,2 (0.2MB (204.8 KBytes))
    Pow = pow_of(1000, Value),
    round(Value * math:pow(1024, Pow) / math:pow(1000, Pow), 10).

-spec get_base_1024_interval(Int :: number(), Min :: number(), Max :: number()) -> float().

get_base_1024_interval(Int, Min, Max) ->
    Interval = convert_to_base_1024(Int),
    AbsMax = lists:max([abs(Min), abs(Max)]),

    AbsMaxPow = pow_of(1000, AbsMax),
    IntPow = pow_of(1000, Int),
    if
        AbsMaxPow == IntPow ->
            Interval;
        IntPow < 0 ->
            round(Interval * 1.024, 10);
        true ->
            round(Interval * 1.024, 2)
    end.

-spec convert_units(Unixtime :: non_neg_integer(), unixtime) -> string().

convert_units(Unixtime, unixtime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(unixtime_to_erlangtime(Unixtime)),
    sprintf("~4..0B.~2..0B.~2..0B ~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Min, Sec]).

-spec convert_units(Value :: number(), Units :: string(), ConvertType :: with_units | no_units, ValueType :: decimal | binary, 
    Pow :: undefined | non_neg_integer(), Length :: undefined | non_neg_integer) -> string().

convert_units(Value, Units, ConvertType, ValueType, Pow, Length) ->
    BlackList = ["%", "ms", "rpm", "RPM"],
    IsUnitsBlackListed = lists:member(Units, BlackList),
    Abs = abs(Value),
    V4 = round(Value, 4),
    V6 = round(Value, 6),
    Step = case ValueType == binary of true -> 1024; false -> 1000 end,
    Precision = case Abs >= 0.01 of true -> 2; false -> 6 end,
    R = if 
        IsUnitsBlackListed orelse (Units == "" andalso ConvertType =:= with_units) ->
            strip_trailing_zeros(sprintf("~." ++ integer_to_list(Precision) ++ "..f", [V6])) ++ " " ++ Units;
        Abs < 1 andalso is_integer(Length) andalso V4 /= 0 ->
            sprintf("~." ++ integer_to_list(Length) ++ "..f", [V4]) ++ " " ++ Units;
        Abs < 1 ->
            strip_trailing_zeros(sprintf("~.4..f", [V4])) ++ " " ++ Units;
        true ->
            P = case (Pow == undefined orelse Value == 0) of true -> pow_of(Step, Value); false -> Pow end,
            V = round(Value / math:pow(Step, P), 2),
            Vs = strip_trailing_zeros(sprintf("~.2..f", [V])),
            case is_integer(Length) andalso Length > 0 of 
                true -> 
                    sprintf("~." ++ integer_to_list(Length) ++ "..f", [V]) ++ " " ++ pow_to_prefix(P) ++ Units;
                false ->
                    sprintf("~s", [Vs]) ++ " " ++ pow_to_prefix(P) ++ Units
            end
    end,
    string:strip(R, right, 32).

-spec calc_max_length_after_dot(List :: [string()]) -> non_neg_integer().

calc_max_length_after_dot(List) ->
    F = fun(S) -> 
        T = string:tokens(hd(string:tokens(S, " ")), "."),
        case T of [_,X] -> length(X); _ -> 0 end 
    end,
    lists:max([0 | [ F(E) || E <- List ]]).
