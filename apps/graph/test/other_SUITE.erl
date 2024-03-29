-module(other_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        strip_trailing_zeros,
        groupByX,
        ceiling,
        floor,
        round,
        calc_min,
        calc_max,
        calc_units,
        calc_type
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

groupByX(_Config) ->
    [{1, [1,2,4,8]}, {2, [3,12]}, {5, [1]}] = graph:groupByX([{1,1}, {1,2}, {1,4}, {2,3}, {2,12}, {1,8}, {5, 1}]).

ceiling(_Config) ->
    5 = graph:ceiling(4.3),
    10 = graph:ceiling(9.999),
    -3 = graph:ceiling(-3.14),
    4 = graph:ceiling(4).

floor(_Config) ->
    4 = graph:floor(4.3),
    9 = graph:floor(9.999),
    -4 = graph:floor(-3.14),
    4 = graph:floor(4).

round(_Config) ->
    4.1 = graph:round(4.11, 1),
    0.03 = graph:round(0.030003, 3).

strip_trailing_zeros(_Config) ->
    "4.1" = graph:strip_trailing_zeros("4.10000"),
    "12" = graph:strip_trailing_zeros("12.0000000"),
    "-3" = graph:strip_trailing_zeros("-3"),
    "-3.001" = graph:strip_trailing_zeros("-3.00100").

calc_min(_Config) ->
    2 = graph:calc_min([[{<<"data">>, [[3,10], [4,5], [5, 2]]}]]),
    undefined = graph:calc_min([]).

calc_max(_Config) ->
    10 = graph:calc_max([[{<<"data">>, [[3,10], [4,5], [5, 2]]}]]),
    undefined = graph:calc_max([]).

calc_type(_Config) ->
    binary = graph:calc_type([[{<<"data">>, []}, {<<"units">>, <<"B">>}]]),
    decimal = graph:calc_type([[{<<"units">>, "q/s"}]]),
    binary = graph:calc_type([
        [{<<"data">>, []}, {<<"units">>, <<"B">>}],
        [{<<"data">>, []}, {<<"units">>, "Bps"}]]).

calc_units(_Config) ->
    "Bps" = graph:calc_units([[{<<"data">>, []}, {<<"units">>, <<"Bps">>}], [{<<"data">>, []}, {<<"units">>, <<"Bps">>}]]),
    "" = graph:calc_units([[{<<"data">>, []}, {<<"units">>, <<"Bps">>}], [{<<"data">>, []}, {<<"units">>, <<"pps">>}]]).
