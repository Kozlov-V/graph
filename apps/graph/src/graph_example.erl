-module(graph_example).
-export([graph1/0]).

graph1() ->
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),

    SizeX = 900,
    ShiftXleft = 85,
    ShiftXright = 30,
    SizeY = 200,
    ShiftY = 36,
    LegendOffsetY = 80,

    Width = SizeX + ShiftXleft + ShiftXright,
    Height = SizeY + ShiftY + LegendOffsetY,

    {ok, G} = gd:new(),
    {ok, Index} = gd:image_create_true_color(G, Width, Height),

    {ok, BackgroundColor} = gd:image_color_allocate(G, Index, 16#F0, 16#F0, 16#F0),
    {ok, GraphBorderColor} = gd:image_color_allocate(G, Index, 16#22, 16#22, 16#22),

    ok = gd:image_filled_rectangle(G, Index, 0, 0, Width, Height, BackgroundColor),
    ok = gd:image_rectangle(G, Index, 0, 0, Width - 1, Height - 1, GraphBorderColor),

    {ok, Binary} = gd:image_png_ptr(G, Index),
    gd:stop(G),
    file:write_file("/tmp/a.png", Binary).

    

