-module(other_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        groupByX,
        ceiling,
        floor
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
