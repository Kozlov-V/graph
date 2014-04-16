-module(convert_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        unixtime,
        decimal_with_units,
        decimal_without_units,
        decimal_ignore_units,
        small_percents,
        small_values_with_undefined_length,
        precision_defined,
        binary_ignore_units
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

decimal_with_units(_Config) ->
    "99.55 %" = graph:convert_units(99.5485, "%", with_units, decimal, undefined, undefined).

decimal_without_units(_Config) ->
    "6950" = graph:convert_units(6950, "", with_units, decimal, undefined, undefined).

decimal_ignore_units(_Config) ->
    "10 K" = graph:convert_units(10000, "", no_units, decimal, undefined, undefined),
    "21.51 Kpps" = graph:convert_units(21508, "pps", no_units, decimal, undefined, undefined),
    "1.0" = graph:convert_units(1.0000000, "", no_units, decimal, undefined, 1).

binary_ignore_units(_Config) ->
    "220.0 GB" = graph:convert_units(236223201280, "B", no_units, binary, undefined, 1),
    "10 G" = graph:convert_units(10737418240.0, "", no_units, binary, undefined, undefined),
    "5.9 GB" = graph:convert_units(6335076761.6000000, "B", no_units, binary, undefined, 1).

small_percents(_Config) ->
    "0.09 %" = graph:convert_units(0.0876, "%", no_units, decimal, undefined, undefined).

precision_defined(_Config) ->
    "0.010" = graph:convert_units(0.01, "", no_units, decimal, undefined, 3).

small_values_with_undefined_length(_Config) ->
    "0.6" = graph:convert_units(0.6000000, "", no_units, decimal, undefined, undefined).
