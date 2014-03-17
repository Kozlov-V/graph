-module(graph_example).
-export([graph1/0, graph2/0, graph3/0]).

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

graph3() ->
    Fun = fun(Gd, D) ->
        Width = get_width(D),
        Height = get_height(D),

        Color = palette(Gd),
        draw_rectangle(Gd, Width, Height, Color(background), Color(graphborder)),
        draw_header(Gd, D(fontpath), D(header), Color(text), Width),
        draw_work_period(Gd, D(shiftXleft) + 1, D(shiftY), D(sizeX) + D(shiftXleft) - 1, D(sizeY) + D(shiftY), Color(graph))
    end,
    graph(Fun, "/tmp/c.png").

graph(Fun, FilePath) ->
    ok = erl_ddll:load_driver("deps/elib_gd/priv/", "elib_gd_drv"),
    D = default(),

    Width = get_width(D),
    Height = get_height(D),

    {ok, G} = gd:new(),
    {ok, Index} = gd:image_create_true_color(G, Width, Height),
    Gd = {G, Index},

    Fun(Gd, D),
    {ok, Binary} = gd:image_png_ptr(G, Index),
    gd:stop(G),
    file:write_file(FilePath, Binary).

default() ->
    L = [
        {sizeX, 900},
        {shiftXleft, 85},
        {shiftXright, 30},
        {sizeY, 200},
        {shiftY, 36},
        {legendOffsetY, 80},
        {header, "mylogin : simple graph"},
        {fontpath, "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf"}
    ],
    fun(P) -> proplists:get_value(P, L) end.

get_width(D) when is_function(D) ->
    D(sizeX) + D(shiftXleft) + D(shiftXright).

get_height(D) when is_function(D) ->
    D(sizeY) + D(shiftY) + D(legendOffsetY).

palette({Gd, Index}) ->
    Colors = [
        {background, {16#F0, 16#F0, 16#F0}},
        {graphborder, {16#22, 16#22, 16#22}},
        {text, {16#20, 16#20, 16#20}},
        {graph, {16#FF, 16#FF, 16#FF}}
    ],
    L = [ begin {ok, C} = gd:image_color_allocate(Gd, Index, R, G, B), {T, C} end || {T, {R, G, B}} <- Colors],
    fun(C) -> proplists:get_value(C, L) end.

draw_rectangle({Gd, Index}, Width, Height, BackGroundColor, GraphBorderColor) ->
    ok = gd:image_filled_rectangle(Gd, Index, 0, 0, Width, Height, BackGroundColor),
    ok = gd:image_rectangle(Gd, Index, 0, 0, Width - 1, Height - 1, GraphBorderColor).

draw_work_period({Gd, Index}, Left, Top, Right, Bottom, GraphColor) ->
    gd:image_filled_rectangle(Gd, Index, Left, Top, Right, Bottom, GraphColor).

draw_header({Gd, Index}, FontPath, Text, TextColor, Width) ->
    PossibleFonts = [ gd_font:factory(FontPath, FontSize) || FontSize <- lists:seq(8, 11) ],
    Font = lists:last(lists:takewhile(fun(F) -> {ok, W} = gd:text_width(Gd, F, Text), W =< Width end, PossibleFonts)),
    {ok, TextWidth} = gd:text_width(Gd, Font, Text),
    Xheader = trunc((Width - TextWidth) / 2),
    Yheader = 24,

    gd:image_string_ft(Gd, Index, TextColor, Font, 0, Xheader, Yheader, Text).

