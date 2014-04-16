-module(other_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        groupByX
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
