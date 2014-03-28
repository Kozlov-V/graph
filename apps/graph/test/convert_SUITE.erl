-module(convert_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        unixtime
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

unixtime(_Config) ->
    "1970.01.01 03:00:00" = graph:convert_units(0, unixtime),
    "1976.05.03 22:33:20" = graph:convert_units(200000000, unixtime),
    "1982.09.04 19:06:40" = graph:convert_units(400000000, unixtime),
    "1995.05.09 10:13:20" = graph:convert_units(800000000, unixtime),
    "2001.09.09 05:46:40" = graph:convert_units(1000000000, unixtime),
    "2008.01.11 00:20:00" = graph:convert_units(1200000000, unixtime),
    "2014.05.13 20:53:20" = graph:convert_units(1400000000, unixtime),
    "2014.03.20 23:00:25" = graph:convert_units(1395342025, unixtime).
