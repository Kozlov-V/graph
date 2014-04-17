-module(calc_hgrid_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        calc_min_max_interval
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

calc_min_max_interval(_Config) ->
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, decimal),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, decimal),
    {0.0, 50000.0000000, 10000.0} = graph:calc_min_max_interval(3039, 43833.0000000, 40, 200, decimal),
    {0.0, 80.0000000, 20.0} = graph:calc_min_max_interval(3.7363, 79.4483000, 40, 200, decimal),
    {0.0000, 0.0025000, 0.0005} = graph:calc_min_max_interval(0.0000, 0.0021000, 40, 200, decimal),
    {0.0, 30.0000000, 5.0} = graph:calc_min_max_interval(4.0297, 28.7531000, 40, 200, decimal),
    {0.0, 30.0000000, 5.0} = graph:calc_min_max_interval(0.0000, 27.9402000, 40, 200, decimal),
    {0.30, 0.6500000, 0.05} = graph:calc_min_max_interval(0.3022, 0.6060000, 40, 200, decimal),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, decimal),
    {2.0, 14.0000000, 2.0} = graph:calc_min_max_interval(3.7485, 12.8697000, 40, 200, decimal),
    {30.0, 90.0000000, 10.0} = graph:calc_min_max_interval(37.3987, 82.8941000, 40, 200, decimal),
    {5000.0, 7500.0000000, 500.0} = graph:calc_min_max_interval(5352, 7445.0000000, 40, 200, decimal),
    {0.0, 50.0000000, 10.0} = graph:calc_min_max_interval(0.0138, 45.5775000, 40, 200, decimal),
    {0.8, 1.3000000, 0.1} = graph:calc_min_max_interval(0.8175, 1.2388000, 40, 200, decimal),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, decimal),
    {4294967296.000000, 4939212390.4000000, 107374182.400000} = graph:calc_min_max_interval(4615254016, 4615630848.0000000, 40, 200, binary),
    {39.0, 44.0000000, 1.0} = graph:calc_min_max_interval(41.3974, 41.4019000, 40, 200, decimal),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, binary),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, decimal),
    {204010946560.0000000, 246960619520.0000000, 5368709120.0000000000} = graph:calc_min_max_interval(219335962624, 231822381056.0000000, 40, 200, binary),
    {44.0, 51.0000000, 1.0} = graph:calc_min_max_interval(47.1395, 47.9229000, 40, 200, decimal),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, binary),
    {6335076761.600000, 7194070220.8000000, 107374182.400000} = graph:calc_min_max_interval(6749532160, 6749908992.0000000, 40, 200, binary),
    {68719476736.0000000, 83751862272.0000000, 2147483648.0000000000} = graph:calc_min_max_interval(73198252032, 78493192192.0000000, 40, 200, binary),
    {236223201280.0000000, 268435456000.0000000, 5368709120.0000000000} = graph:calc_min_max_interval(249077264384, 252824371200.0000000, 40, 200, binary),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, binary),
    {90.0, 102.0000000, 2.0} = graph:calc_min_max_interval(96.7167, 96.7900000, 40, 200, decimal),
    {0.000000, 2147483648.0000000, 536870912.000000} = graph:calc_min_max_interval(2096099328, 2096099328.0000000, 40, 200, binary),
    {960.0, 1080.0000000, 20.0} = graph:calc_min_max_interval(1028.0756, 1028.1903000, 40, 200, decimal),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, binary),
    {0.000000, 1468006.4000000, 209715.200000} = graph:calc_min_max_interval(1429504, 1429504.0000000, 40, 200, binary),
    {94.0, 106.0000000, 2.0} = graph:calc_min_max_interval(99.5633, 99.5637000, 40, 200, decimal),
    {0.0, 100.0000000, 20.0} = graph:calc_min_max_interval(99.9985, 99.9985000, 40, 200, decimal),
    {0.0, 1.2000000, 0.2} = graph:calc_min_max_interval(undefined, undefined, 40, 200, decimal),
    {0.0000000, 614400.0000000, 102400.0000000000} = graph:calc_min_max_interval(512000, 512000.0000000, 40, 200, binary),
    {16000000.0, 18000000.0000000, 200000.0} = graph:calc_min_max_interval(16992745, 17031884.0000000, 40, 200, decimal),
    {8.0, 16.0000000, 1.0} = graph:calc_min_max_interval(8.2229, 15.1198000, 40, 200, decimal),
    {84000000.0, 94000000.0000000, 2000000.0} = graph:calc_min_max_interval(89152760, 89190564.0000000, 40, 200, decimal),
    {19000000000.0, 21500000000.0000000, 500000000.0} = graph:calc_min_max_interval(20414132345, 20442653268.0000000, 40, 200, decimal),
    {1440000000.0, 1620000000.0000000, 20000000.0} = graph:calc_min_max_interval(1528471049, 1532129950.0000000, 40, 200, decimal),
    {6000000.0, 22000000.0000000, 2000000.0} = graph:calc_min_max_interval(7944848, 20236960.0000000, 40, 200, decimal),
    {0.0, 2500000.0000000, 500000.0} = graph:calc_min_max_interval(2459110, 2459110.0000000, 40, 200, decimal).
