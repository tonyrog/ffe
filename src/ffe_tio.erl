%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Ffe io
%%% @end
%%% Created : 29 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(ffe_tio).

-export([output_ready/1]).
-export([output/2]).
-export([output_sync/2]).
-export([input_ready/1]).
-export([input/2]).
-export([get_line/1]).

-include("ffe.hrl").

output(?TERMINAL_OUTPUT, Cs) ->
    ffe_tty:output(ffe:tty(), Cs);
output(?STANDARD_OUTPUT, Cs) ->
    erlang:display_string(lists:flatten(Cs));
output(Fd, Cs) when is_integer(Fd), Fd > 2 ->
    file:write(ffe:get_fd(Fd), Cs).

output_sync(?TERMINAL_OUTPUT, Cs) ->
    ffe_tty:output_sync(ffe:tty(), Cs);
output_sync(?STANDARD_OUTPUT, Cs) ->
    erlang:display_string(lists:flatten(Cs));
output_sync(Fd, Cs) when is_integer(Fd), Fd > 2 ->
    file:write(ffe:get_fd(Fd), Cs).

output_ready(undefined) ->
    false;
output_ready(_) ->
    true.

input_ready(?TERMINAL_INPUT) ->
    ffe_tty:input_ready(ffe:tty());
input_ready(Fd) when Fd >= 0 ->
    true.

input(_, 0) ->
    [];
input(?TERMINAL_INPUT, N) when N >= 1 ->
    read_n_chars(ffe:tty(), N, []);
input(?STANDARD_INPUT, N) ->
    io:get_chars(standard_io, N);
input(Fd, N) when is_integer(Fd), Fd > 2, is_integer(N), N >= 1 ->
    case file:read(ffe:get_fd(Fd), N) of
	{ok,Data} when is_list(Data) -> Data;
	{ok,Data} when is_binary(Data) -> binary_to_list(Data);
	eof -> eof
    end.

read_n_chars(_TTY, 0, Acc) ->
    lists:reverse(Acc);
read_n_chars(TTY, N, Acc) ->
    case ffe_tty:input(TTY) of
	eof when Acc =:= [] -> eof;
	eof -> lists:reverse(Acc);
	Key -> read_n_chars(TTY, N-1, [Key|Acc])
    end.

%% read line of data, return binary without the line terminator
-spec get_line(Fd::integer()) -> binary().

get_line(?TERMINAL_INPUT) ->
    ffe_tty:get_line(ffe:tty());
get_line(?STANDARD_INPUT) ->
    case io:get_line(standard_io, '') of
	eof ->  eof;
	Line -> strip_nl(Line)
    end;
get_line(Fd) when is_integer(Fd), Fd > 2 ->
    case file:read_line(ffe:get_fd(Fd)) of
	eof -> eof;
	{ok,Line} -> strip_nl(Line)
    end.

strip_nl(Line) when is_binary(Line) ->
    LineSize1 = byte_size(Line)-1,
    LineSize2 = LineSize1-1,
    case Line of
	<<Line2:LineSize2/binary,$\r,$\n>> -> Line2;
	<<Line1:LineSize1/binary,$\n>> -> Line1;
	_ -> Line
    end;
strip_nl(Line) when is_list(Line) ->
    strip_nl(list_to_binary(Line)).

