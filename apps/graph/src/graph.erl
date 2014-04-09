-module(graph).
-compile(export_all).

-define(SEC_PER_HOUR, 3600).
-define(SEC_PER_DAY, 86400).
-define(FONT_SIZES, [8,9,10,11]).

draw_rectangle(Dim, Palette) ->
    fun({Gd, Index}) ->
        ok = gd:image_filled_rectangle(Gd, Index, 0, 0, Dim(width), Dim(height), Palette(background)),
        ok = gd:image_rectangle(Gd, Index, 0, 0, Dim(width) - 1, Dim(height) - 1, Palette(graphborder))
    end.

draw_header(Dim, Palette, FontPath, Text) ->
    fun({Gd, Index}) ->
        PossibleFonts = [ gd_font:factory(FontPath, FontSize) || FontSize <- ?FONT_SIZES ],
        Font = lists:last(lists:takewhile(fun(F) -> {ok, W} = gd:text_width(Gd, F, Text), W =< Dim(width) end, PossibleFonts)),
        {ok, TextWidth} = gd:text_width(Gd, Font, Text),
        Xheader = trunc((Dim(width) - TextWidth) / 2),
        Yheader = 24,
        {ok, _} = gd:image_string_ft(Gd, Index, Palette(text), Font, 0, Xheader, Yheader, Text)
    end.

draw_work_period(Dim, Palette) ->
    fun({Gd, Index}) ->
        ok = gd:image_filled_rectangle(Gd, Index, Dim(shiftXleft) + 1, Dim(shiftY), 
            Dim(sizeX) + Dim(shiftXleft) - 1, Dim(sizeY) + Dim(shiftY), Palette(graph))
    end.

mapX(Dim, From, Period) ->
    fun(T) ->
        trunc(Dim(shiftXleft) + (T - From) * Dim(sizeX) / Period)
    end.

mapY(Dim, Y0, Ytop) ->
    fun(Y) ->
        trunc(Dim(sizeY) + Dim(shiftY) - (Y - Y0) * Dim(sizeY) / (Ytop - Y0))
    end.

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
