-module(calc_hgrid_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        base_1000,
        base_1024,
        close_min_and_max,
        same_min_and_max
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

base_1000(_Config) ->
    {15.0, 30.0, 5.0} = graph:calc_horizontal_grid(19, 29, 40/90),
    {11000.0, 18000.0, 1000.0} = graph:calc_horizontal_grid(11148, 17489, 40/200),
    {5.0, 40.0, 5.0} = graph:calc_horizontal_grid(5.8714, 39.902, 40/200),
    {0.0, 0.0025, 0.0005} = graph:calc_horizontal_grid(0, 0.0021, 40/200),
    {0.0, 35000.0, 5000.0} = graph:calc_horizontal_grid(0, 31882, 40/200),
    {0.0, 80.0, 20.0} = graph:calc_horizontal_grid(0, 78.3816000, 40/200),
    {15.0, 30.0, 5.0} = graph:calc_horizontal_grid(19, 29, 40/90),
    {0.0, 30.0, 5.0} = graph:calc_horizontal_grid(0, 27.9402, 40/200),
    {5500.0, 8500.0, 500.0} = graph:calc_horizontal_grid(5642, 8084.0000000, 40/200),
    ok.

base_1024(_Config) ->
    {4080218931.2000003, 5368709120.0, 214748364.8} = graph:calc_horizontal_grid(4470066585.60, 4941171916.8000000, 40/200, binary),
    {209379655680.0, 252329328640.0, 5368709120.0} = graph:calc_horizontal_grid(212727195648.0, 249602144870.4, 40/200, binary),
    ok.

close_min_and_max(_Config) ->
    {2650000.0, 2950000.0, 50000.0} = graph:calc_horizontal_grid(2807952.0, 2808232, 40/200).

same_min_and_max(_Config) ->
    {0.0, 204800.0, 51200.0} = graph:calc_horizontal_grid(196608.0, 196608.0, 40/200, binary).
