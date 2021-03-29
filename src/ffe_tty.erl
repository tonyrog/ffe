%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Handle tty io
%%% @end
%%% Created : 22 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(ffe_tty).

-export([open/0]).
-export([input/1]).
-export([input_ready/1]).
-export([output/2]).
-export([output_ready/1]).
-export([output_sync/2]).
-export([get_line/1]).
-export([move/2]).
-export([insert/2]).
-export([delete/2]).
-export([beep/1]).
-export([get_tty_geometry/1]).
-export([get_unicode_state/1]).
-export([set_unicode_state/2]).

-define(OP_PUTC,0).
-define(OP_MOVE,1).
-define(OP_INSC,2).
-define(OP_DELC,3).
-define(OP_BEEP,4).
-define(OP_PUTC_SYNC,5).
% Control op
-define(ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER, 16#018b0900).
-define(CTRL_OP_GET_WINSIZE, (100 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).
-define(CTRL_OP_GET_UNICODE_STATE, (101 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).
-define(CTRL_OP_SET_UNICODE_STATE, (102 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).
-define(ESC,   27).
-define(UP,    $A).
-define(DOWN,  $B).
-define(RIGHT, $C).
-define(LEFT,  $D).

-define(DEL,   $3).

-define(FFE_TTY_INPUT, ffe_tty_input).
-define(FFE_KILL_BUFFER, ffe_kill_buffer).

%% FIXME: how do we steel tty_sl without fuzz?
open() ->
    put(?FFE_TTY_INPUT, []),
    open_port({spawn,'tty_sl -c -e'}, [eof]).

input(Port) ->
    case get(?FFE_TTY_INPUT) of
	[Key|Ks] ->
	    put(?FFE_TTY_INPUT, Ks),
	    Key;
	[] ->
	    receive
		{Port,{data,Cs}} ->
		    [Key|Ks] = translate_keys(Cs),
		    put(?FFE_TTY_INPUT, Ks),
		    Key;
		{Port,eof} ->
		    eof
	    end
    end.

input_ready(Port) ->
    case get(?FFE_TTY_INPUT) of
	[_|_] ->
	    true;
	[] ->
	    receive
		{Port,{data,Cs=[_|_]}} ->
		    Ks = translate_keys(Cs),
		    put(?FFE_TTY_INPUT, Ks),
		    true
	    after 0 ->
		    false
	    end
    end.

output_ready(undefined) ->
    false;
output_ready(_) ->
    true.

output(Port, Cs) ->
    port_command(Port, [?OP_PUTC|unicode:characters_to_binary(Cs,utf8)]).   

output_sync(Port, Cs) ->
    port_command(Port,[?OP_PUTC_SYNC|unicode:characters_to_binary(Cs,utf8)]).

move(Port, N) ->
    port_command(Port, [?OP_MOVE|<<N:16>>]).

insert(Port,Cs) ->
    port_command(Port, [?OP_INSC|unicode:characters_to_binary(Cs,utf8)]).

delete(Port, N) ->
    port_command(Port, [?OP_DELC|<<N:16>>]).

beep(Port) ->
    port_command(Port, [?OP_BEEP]).

% Let driver report window geometry,
% definitely outside of the common interface
get_tty_geometry(Port) ->
    case (catch port_control(Port,?CTRL_OP_GET_WINSIZE,[])) of
	List when length(List) =:= 8 -> 
	    <<W:32/native,H:32/native>> = list_to_binary(List),
	    {W,H};
	_ ->
	    error
    end.
get_unicode_state(Port) ->
    case (catch port_control(Port,?CTRL_OP_GET_UNICODE_STATE,[])) of
	[Int] when Int > 0 -> 
	    true;
	[Int] when Int =:= 0 ->
	    false;
	_ ->
	    error
    end.

set_unicode_state(Port, Bool) ->
    Data = case Bool of
	       true -> [1];
	       false -> [0]
	   end,
    case (catch port_control(Port,?CTRL_OP_SET_UNICODE_STATE,Data)) of
	[Int] when Int > 0 -> 
	    {unicode, utf8};
	[Int] when Int =:= 0 ->
	    {unicode, false};
	_ ->
	    error
    end.

%% translate multiple keystrokes to meta keys
translate_keys([?ESC,$[,$A | Cs]) ->
    [up | translate_keys(Cs)];
translate_keys([?ESC,$[,$B | Cs]) ->
    [down | translate_keys(Cs)];
translate_keys([?ESC,$[,$C | Cs]) ->
    [right | translate_keys(Cs)];
translate_keys([?ESC,$[,$D | Cs]) ->
    [left | translate_keys(Cs)];
translate_keys([?ESC,$[,$3,$~ | Cs]) ->
    [delete | translate_keys(Cs)];
translate_keys([?ESC,C | Cs]) ->
    [{esc,C}|translate_keys(Cs)];
translate_keys([C|Cs]) ->
    [C|translate_keys(Cs)];
translate_keys([]) ->
    [].

-define(CTRL_A, $\^a).  %% beginning of line
-define(CTRL_B, $\^b).  %% backward char
-define(CTRL_D, $\^d).  %% delete char
-define(CTRL_E, $\^e).  %% end of line
-define(CTRL_F, $\^f).  %% forward char
-define(CTRL_K, $\^k).  %% kill (cut) until end of line
-define(CTRL_P, $\^p).  %% previous line
-define(CTRL_Y, $\^y).  %% yank(insert) from kill buffer
-define(BACKSPACE, 127).

%% {esc,$b} - backward word
%% {esc,$f} - forward word

get_line(Port) ->
    get_line(Port, [], []).

get_line(Port, After, Before) ->
    case input(Port) of
	eof -> eof;
	$\r ->
	    output(Port, [$\s]),
	    lists:reverse(Before) ++ After;
	$\t ->
	    %% FIXME: expand_fun!
	    {Silent,Insert,Expand} = ffe:expand(Before),
	    if Silent =:= yes -> ok;
	       Silent =:= no -> beep(Port)
	    end,
	    %% FIXME: build into expand fun
	    ffe:format_word_list(0, Expand),
	    Before1 = lists:reverse(binary_to_list(Insert),Before),
	    if Expand =:= [], Insert =/= [] ->
		    insert(Port, Insert);
	       Expand =/= [] ->
		    insert(Port, lists:reverse(Before1));
	       true ->
		    ok
	    end,
	    get_line(Port, After, Before1);
	$\b ->
	    get_line_bs(Port, After, Before);
	?BACKSPACE ->
	    get_line_bs(Port, After, Before);
	?CTRL_A ->
	    get_line_beginning_of_line(Port, After, Before);
	?CTRL_B ->
	    get_line_backward_char(Port, After, Before);
	?CTRL_D ->
	    get_line_delete_char(Port, After, Before);
	?CTRL_E ->
	    get_line_end_of_line(Port, After, Before);
	?CTRL_F ->
	    get_line_forward_char(Port, After, Before);
	?CTRL_K ->
	    get_line_kill_to_end_of_line(Port, After, Before);
	?CTRL_Y ->
	    get_line_insert_from_kill_buffer(Port, After, Before);
	left ->
	    get_line_backward_char(Port, After, Before);
	right ->
	    get_line_forward_char(Port, After, Before);
	Key when Key >= $\s, Key =< $~ ->
	    insert(Port, [Key]),
	    get_line(Port, After, [Key|Before]);
	_Key ->
	    beep(Port),
	    %% io:format("char ~p\n", [Key]),
	    get_line(Port, After, Before)
    end.


get_line_delete_char(Port, After, Before) ->
    case Before of
	[] ->
	    io:beep(Port), %% option?
	    get_line(Port, After, Before);
	[_|Before1] ->
	    delete(Port, -1),
	    get_line(Port, After, Before1)
    end.

get_line_backward_char(Port, After, Before) ->
    case Before of
	[] ->
	    beep(Port), %% option?
	    get_line(Port, After, Before);
	[Char|Before1] ->
	    move(Port, -1),
	    get_line(Port, [Char|After], Before1)
    end.

get_line_forward_char(Port, After, Before) ->
    case After of
	[Char|After1] ->
	    move(Port, 1),
	    get_line(Port, After1, [Char|Before]);
	[] ->
	    beep(Port), %% option?
	    get_line(Port, After, Before)
    end.

get_line_end_of_line(Port, After, Before) ->
    case After of
	[] ->
	    beep(Port),
	    get_line(Port, After, Before);
	_ ->
	    move(Port, length(After)),
	    get_line(Port, [], lists:reverse(After,Before))
    end.
    
get_line_beginning_of_line(Port, After, Before) ->
    case Before of
	[] ->
	    beep(Port),
	    get_line(Port, After, Before);
	_ ->
	    move(Port, -length(Before)),
	    get_line(Port, lists:reverse(Before,After), [])
    end.
    
get_line_bs(Port, After, Before) ->
    case Before of
	[_|Before1] ->
	    delete(Port, -1),
	    get_line(Port, After, Before1);
	[] ->
	    beep(Port),
	    get_line(Port, After, Before)
    end.

get_line_kill_to_end_of_line(Port, After, Before) ->
    case After of
	[] ->
	    put(?FFE_KILL_BUFFER, []),
	    get_line(Port, After, Before);
	_ ->
	    delete(Port, length(After)),
	    put(?FFE_KILL_BUFFER, After),
	    get_line(Port, [], Before)
    end.

get_line_insert_from_kill_buffer(Port, After, Before) ->
    case get(?FFE_KILL_BUFFER) of
	[] ->
	    get_line(Port, After, Before);
	Yank ->
	    insert(Port, Yank),
	    get_line(Port, After, lists:reverse(Yank, Before))
    end.

