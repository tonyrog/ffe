%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Handle (nearly)raw io
%%% @end
%%% Created : 22 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(ffe_io).

-export([open/0]).
-export([input/1]).
-export([input_ready/1]).
-export([output/2]).
-export([output_ready/1]).
-export([output_sync/2]).
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

%% FIXME: how do we steel tty_sl without fuzz?
open() ->
    put(ffe_io_input, []),
    open_port({spawn,'tty_sl -c -e'}, [eof]).

input(Port) ->
    case get(ffe_io_input) of
	[Key|Ks] ->
	    put(ffe_io_input, Ks),
	    Key;
	[] ->
	    receive
		{Port,{data,Cs}} ->
		    [Key|Ks] = translate_keys(Cs),
		    put(ffe_io_input, Ks),
		    Key;
		{Port,eof} ->
		    eof
	    end
    end.

input_ready(Port) ->
    case get(ffe_io_input) of
	[_|_] ->
	    true;
	[] ->
	    receive
		{Port,{data,Cs=[_|_]}} ->
		    Ks = translate_keys(Cs),
		    put(ffe_io_input, Ks),
		    true
	    after 0 ->
		    false
	    end
    end.

output_ready(_Port) ->
    true.

output(Port, Cs) ->
    port_command(Port, [?OP_PUTC|unicode:characters_to_binary(Cs,utf8)]).

output_sync(Port, Cs) ->
    port_command(Port, 
		 [?OP_PUTC_SYNC|unicode:characters_to_binary(Cs,utf8)]).

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
get_tty_geometry(Iport) ->
    case (catch port_control(Iport,?CTRL_OP_GET_WINSIZE,[])) of
	List when length(List) =:= 8 -> 
	    <<W:32/native,H:32/native>> = list_to_binary(List),
	    {W,H};
	_ ->
	    error
    end.
get_unicode_state(Iport) ->
    case (catch port_control(Iport,?CTRL_OP_GET_UNICODE_STATE,[])) of
	[Int] when Int > 0 -> 
	    true;
	[Int] when Int =:= 0 ->
	    false;
	_ ->
	    error
    end.

set_unicode_state(Iport, Bool) ->
    Data = case Bool of
	       true -> [1];
	       false -> [0]
	   end,
    case (catch port_control(Iport,?CTRL_OP_SET_UNICODE_STATE,Data)) of
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
