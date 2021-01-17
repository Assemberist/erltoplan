-module(loop).

-export([loop/0]).

-include("/home/sanya/Рабочий стол/sources/erlang/test/gui_definitions.hrl").

loop() ->
    receive
        #wx{id = ?btn_plus, event = #wxCommand{type = command_button_clicked}} ->
            NewWindow = element_storager:get(?main_form),
            MD = wxMessageDialog:new(NewWindow, "Your result ok",
                [{style, ?wxOK}, {caption, "Game over"}]),
            wxDialog:showModal(MD),
            wxDialog:destroy(MD);
            
        #wx{id = ?btn_sub, event = #wxCommand{type = command_button_clicked}} ->
            TextField = element_storager:get(?textField),
            Text = case wxTextCtrl:getValue(TextField) of
                [] -> [];
                [ _ | Tail] -> Tail
            end,
            wxTextCtrl:setValue(TextField, Text);

        #wx{event = #wxClose{}} ->
            case whereis(?MODULE) of
                undefined -> wx:destroy();
                Childwindow -> Childwindow ! {self(),exit},
                    wx:destroy()
            end,
            exit(ok)
    end,
    loop().