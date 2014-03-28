-module(graph).
-compile(export_all).

-define(SEC_PER_HOUR, 3600).
-define(SEC_PER_DAY, 86400).

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

%% 
timezone(Time) ->
    LocalTime = calendar:now_to_local_time(Time),
    UTCTime = calendar:now_to_universal_time(Time),
    calendar:datetime_to_gregorian_seconds(LocalTime) - calendar:datetime_to_gregorian_seconds(UTCTime).

unixtime_to_erlangtime(Timestamp) ->
    {Timestamp div 1000000, Timestamp rem 1000000, 0}.

date2str(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
