-module(maket).

-export([create_form/0]).
-include("/home/sanya/Рабочий стол/sources/erlang/test/gui_definitions.hrl").

create_form() ->
    element_storager:start(),
    %% main form
    Frame = wxFrame:new(wx:null(), ?main_form, "Match3", [{size, {130, 130}}, {style, ?wxDEFAULT_FRAME_STYLE band (bnot (?wxMAXIMIZE_BOX bor ?wxRESIZE_BORDER))}]),
    element_storager:push(?main_form, Frame),
    wxFrame:connect(Frame, command_button_clicked),
    wxFrame:connect(Frame, close_window),

    TextField = wxTextCtrl:new(Frame, ?textField, [{size, {100, 15}}, {pos, {15, 15}}]),
    element_storager:push(?textField, TextField),

    BtnSum = wxButton:new(Frame, ?btn_plus, [{size, {25, 25}}, {pos, {15, 40}}, {label, "+"}]),
    element_storager:push(?btn_plus, BtnSum),

    BtnSub = wxButton:new(Frame, ?btn_sub, [{size, {25, 25}}, {pos, {40, 40}}, {label, "-"}]),
    element_storager:push(?btn_sub, BtnSub),

    BtnDiv = wxButton:new(Frame, ?btn_divide, [{size, {25, 25}}, {pos, {65, 40}}, {label, "/"}]),
    element_storager:push(?btn_divide, BtnDiv),

    BtnMul = wxButton:new(Frame, ?btn_mul, [{size, {25, 25}}, {pos, {90, 40}}, {label, "*"}]),
    element_storager:push(?btn_mul, BtnMul),

    BtnRezult = wxButton:new(Frame, ?btn_rezult, [{size, {100, 25}}, {pos, {15, 65}}, {label, "="}]),
    element_storager:push(?btn_rezult, BtnRezult),

    wxFrame:show(Frame).
