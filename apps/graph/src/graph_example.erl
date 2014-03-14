-module(graph_example).
-export([graph1/0, graph2/0]).

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

graph2() ->
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),

    SizeX = 900,
    ShiftXleft = 85,
    ShiftXright = 30,
    SizeY = 200,
    ShiftY = 36,
    LegendOffsetY = 80,
    Header = "mylogin : simple graph",
    Font = gd_font:set_font_path(gd_font:new(), "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf"),

    Width = SizeX + ShiftXleft + ShiftXright,
    Height = SizeY + ShiftY + LegendOffsetY,

    {ok, G} = gd:new(),
    {ok, Index} = gd:image_create_true_color(G, Width, Height),

    {ok, BackgroundColor} = gd:image_color_allocate(G, Index, 16#F0, 16#F0, 16#F0),
    {ok, GraphBorderColor} = gd:image_color_allocate(G, Index, 16#22, 16#22, 16#22),
    {ok, TextColor} = gd:image_color_allocate(G, Index, 16#20, 16#20, 16#20),

    ok = gd:image_filled_rectangle(G, Index, 0, 0, Width, Height, BackgroundColor),
    ok = gd:image_rectangle(G, Index, 0, 0, Width - 1, Height - 1, GraphBorderColor),

    PossibleFontSizes = lists:seq(8, 11),
    L1 = [ {S, gd:text_size(G, gd_font:set_point_size(Font, S), Header)} || S <- PossibleFontSizes ],
    {FontSize, {ok, W, _}} = lists:last(lists:takewhile(fun({_, {ok, W, _}}) -> W =< Width end, L1)),
    Xheader = trunc((Width - W) / 2),
    Yheader = 24,
    
    gd:image_string_ft(G, Index, TextColor, gd_font:set_point_size(Font, FontSize), 0, Xheader, Yheader, Header),

    {ok, Binary} = gd:image_png_ptr(G, Index),
    gd:stop(G),
    file:write_file("/tmp/b.png", Binary).

