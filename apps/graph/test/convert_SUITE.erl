-module(convert_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        unixtime,
        bytes
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

bytes(_Config) ->
    "0 B" = graph:convert_units(0.0, 4, "B", undefined),
    "0.2 TB" = graph:convert_units(219902325555.2000120, 4, "B", undefined),
    "0.4 TB" = graph:convert_units(439804651110.4000240, 4, "B", undefined),
    "0.6 TB" = graph:convert_units(659706976665.6000360, 4, "B", undefined),
    "0.8 TB" = graph:convert_units(879609302220.8000480, 4, "B", undefined),
    "1 TB" = graph:convert_units(1099511627776.0000600, 4, "B", undefined),
    "931.48 GB" = graph:convert_units(1000171708416.0000, 3, "B", 2),
    "2 GB" = graph:convert_units(2147483648.0000000, 3, "B", undefined),
    "10 GB" = graph:convert_units(10737418240.0000000, 3, "B", undefined),
    "9.77 GB" = graph:convert_units(10486808576.0, 3, "B", 2).
