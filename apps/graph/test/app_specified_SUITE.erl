-module(app_specified_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        calc_max_length_after_dot
    ].

init_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

end_per_suite(Config) ->
    Config.

calc_max_length_after_dot(_Config) ->
    0 = graph:calc_max_length_after_dot(["0","100 M","200 M","300 M","400 M","500 M"]),
    0 = graph:calc_max_length_after_dot(["0","100 М","200 М","300 М","400 М","500 М"]),
    0 = graph:calc_max_length_after_dot(["0","200","400","600","800"]),
    0 = graph:calc_max_length_after_dot(["0","5","10","15","20","25","30","35"]),
    0 = graph:calc_max_length_after_dot(["0","5 K","10 K","15 K","20 K","25 K"]),
    0 = graph:calc_max_length_after_dot(["0","5 M","10 M","15 M","20 M","25 M","30 M"]),
    0 = graph:calc_max_length_after_dot(["40","45","50","55","60","65","70","75","80"]),
    0 = graph:calc_max_length_after_dot(["4 G","6 G","8 G","10 G","12 G","14 G","16 G","18 G","20 G"]),
    0 = graph:calc_max_length_after_dot(["5 G","6 G","7 G","8 G","9 G","10 G","11 G","12 G","13 G"]),
    0 = graph:calc_max_length_after_dot(["6 M","8 M","10 M","12 M","14 M","16 M","18 M","20 M","22 M","24 M","26 M"]),
    0 = graph:calc_max_length_after_dot(["8","10","12","14","16","18"]),
    0 = graph:calc_max_length_after_dot(["8 M","10 M","12 M","14 M","16 M","18 M","20 M","22 M","24 M","26 M"]),
    1 = graph:calc_max_length_after_dot(["0","0.2","0.4","0.6","0.8","1","1.2"]),
    1 = graph:calc_max_length_after_dot(["0","0.2 G","0.4 G","0.6 G","0.8 G","1 G"]),
    0 = graph:calc_max_length_after_dot(["215 G","220 G","225 G","230 G","235 G","240 G","245 G"]),
    4 = graph:calc_max_length_after_dot(["0","0.0005","0.001","0.0015","0.002","0.0025"]).
