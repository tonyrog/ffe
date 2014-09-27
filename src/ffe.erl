
-module(ffe).

-export([interpret/0, interpret/1, interpret/2]).
-export([test1/0]).
%% -export([create/2]).
%% -compile(export_all).

-define(BYE,       bye).    %% fixme

-define(STACK_OVERFLOW,  -3).
-define(STACK_UNDERFLOW, -4).    %% case clause?
-define(QUIT,            -56).
-define(INTRRUPT,        -28).   %% user interrupt


test1() ->
    interpret(": fifthteen 3 5 * ; fifthteen fifthteen * . exit").

%% temporary hack to test the idea
interpret() ->
    interpret("", dictionary()).

interpret(String) ->
    interpret(String, dictionary()).

interpret(Line, Dict) ->
    interpret(string:tokens(Line, " \t\r\n"), Dict, []).

interpret([":",Name|Ts], Dict, Sp) ->
    compile(Name, Ts, Dict, [], Sp);
interpret(["constant",Name|Ts], Dict, [Value|Sp]) ->
    %% not correct, we must compile into (docon) using W pointer
    %% the constant should be locate at WP
    Word = create(Name, {fun lit/4, Value, fun semis/4}),
    interpret(Ts, maps:put(Name,Word,Dict), Sp);
interpret(["user",Name|Ts], Dict, [Value|Sp]) ->
    %% not correct, we must compile into (douser) using W
    Word = create(Name, {fun lit/4, {user,Value}, fun semis/4}),
    interpret(Ts, maps:put(Name,Word,Dict), Sp);


interpret([T|Ts], Dict, Sp) ->
    io:format("~s : ~w\n", [T, Sp]),
    case maps:find(T, Dict) of
	error ->
	    try list_to_integer(T) of
		Value -> interpret(Ts,Dict,[Value|Sp])
	    catch
		error:_ -> exit({undefined,T})
	    end;
	{ok,W} ->
	    %% only testing now!
	    try next(Sp,[],1,{W,fun ret/4}) of
		Sp1 -> interpret(Ts, Dict, Sp1)
	    catch
		throw:{?BYE=_Code,_Reason} ->
		    Sp;
		throw:{?QUIT=_Code,_Reason} ->
		    interpret(Ts, Dict, Sp);
		throw:{Code,Reason} ->
		    io:format("~w : ~w\n", [Code, Reason]),
		    interpret(Ts, Dict, Sp)
	    end
    end;
interpret([], Dict, Sp) ->
    io:format("ok "),
    case io:get_line('') of
	eof -> Sp;
	Line ->
	    interpret(string:tokens(Line, " \t\r\n"), Dict, Sp)
    end.

%% compilation mode
%% 
compile(Name, [";"|Ts], Dict, Acc, Sp) ->
    Code = list_to_tuple(lists:reverse([fun semis/4 | Acc])),
    io:format("~s = ~w\n", [Name,Code]),
    Word = create(Name,Code),
    interpret(Ts, maps:put(Name,Word,Dict), Sp);
compile(Name, [T|Ts], Dict, Acc, Sp) ->
    io:format("~s ", [T]),
    case maps:find(T, Dict) of
	error ->
	    try list_to_integer(T) of
		Value ->
		    compile(Name,Ts,Dict,[Value,fun lit/4|Acc], Sp)
	    catch
		error:_ ->
		    throw({-13, undefined_word})
	    end;
	{ok,W} ->
	    compile(Name,Ts,Dict,[W|Acc],Sp)
    end.

%% given a tuple of new code create function to execute it
create(Name,NewCode) ->
    fun (_Sp,_Rp,-1,_Code) ->
	    %% way of keeping name in environment, and a method of access it
	    Name; 
	(Sp,Rp,I,Code) ->
	    next(Sp, [{I,Code}|Rp], 1, NewCode)
    end.

dictionary() ->
    #{
       "(semis)" => fun semis/4,
       %% "(docol)" => fun docol/4,
       %% "(douser)" => fun docol/4,
       "(do)" => fun pdo/4,
       "(loop)" => fun ploop/4,
       "@" => fun fetch/4,
       "!" => fun store/4,
       "sp@" => fun spat/4,
       "rp@" => fun rpat/4,
       "sp!" => fun spstore/4,
       "rp!" => fun rpstore/4,
       "i" => fun i/4,
       "j" => fun j/4,
       "leave" => fun leave/4,
       "." => fun dot/4,
       "quit" => fun quit/4,
       "bye" => fun bye/4,
       "noop" => fun noop/4,
       "literal" => fun lit/4,
       "rot" => fun rote/4,
       "-rot" =>  fun rev_rote/4,
       "branch" => fun branch/4,
       "0branch" => fun zbranch/4,
       "+" => fun plus/4,
       "*" => fun star/4,
       "/" => fun slash/4,
       "mod" => fun mod/4,
       "/mod" => fun slash_mod/4,
       "negate" => fun negate/4,
       "over" => fun over/4,
       "drop" => fun drop/4,
       "swap" => fun swap/4,
       "dup" => fun dupe/4,
       "abs" => fun abs/4,
       "and"  => fun 'and'/4,
       "invert"  => fun 'invert'/4,
       "or"  => fun 'or'/4,
       "xor"  => fun 'xor'/4,
       "min" => fun min/4,
       "max" => fun max/4
     }.

next(Sp,Rp,I,Code) ->
    F = element(I, Code),
    F(Sp,Rp,I+1,Code).

pdo([Index,Limit|Sp],Rp,I,Code) ->
    next(Sp, [Index,Limit|Rp], I, Code).

ploop(Sp,[Index,Rp=[Limit|_Rp0]],I,Code) when Index+1 < Limit ->
    next(Sp,[Index+1|Rp],I+element(I,Code),Code);
ploop(Sp,[_Index,_Limit|Rp],I,Code) ->
    next(Sp,Rp,I+1,Code).
    
i(Sp,[Ix|_]=Rp,I,Code) ->
    next([Ix|Sp],Rp,I,Code).

j(Sp,[_,_,Jx|_]=Rp,I,Code) ->
    next([Jx|Sp],Rp,I,Code).

fetch([Addr|Sp],Rp,I,Code) ->
    case Addr of
	{user,_} ->
	    Value = case get(Addr) of
			undefined -> 0;
			V -> V
		    end,
	    next([Value|Sp],Rp,I,Code);
	{sys,_} ->
	    Value = case ets:lookup(forth,Addr) of
			[] -> 0;
			[{_,V}] -> V
		    end,
	    next([Value|Sp],Rp,I,Code)
    end.

store([Addr,Value|Sp],Rp,I,Code) ->
    case Addr of
	{user,_} -> 
	    put(Addr,Value),
	    next(Sp,Rp,I,Code);
	{sys,_} ->
	    ets:insert(forth,{Addr,Value}),
	    next(Sp,Rp,I,Code)
    end.

leave(Sp,[_,Rp=[Limit|_]],I,Code) ->
    next(Sp,[Limit|Rp],I,Code).

quit(_Sp,_Rp,_I,_Code) ->
    throw({?QUIT, quit}).

bye(_Sp,_Rp,_I,_Code) ->
    throw({?BYE, exit}).

dot([Value|Sp],Rp,I,Code) ->
    erlang:display_string(integer_to_list(Value)),
    erlang:display_string(" "),
    next(Sp,Rp,I,Code).

ret(Sp,_Rp,_I,_Code) ->
    Sp.

semis(Sp,[{I,Code}|Rp],_I,_Code) ->
    next(Sp,Rp,I,Code).


noop(Sp,Rp,I,Code) ->
    next(Sp,Rp,I,Code).

lit(Sp,Rp,I,Code) when is_integer(I), I>0 ->
    next([element(I,Code)|Sp],Rp,I+1,Code).

rote([A,B,C|Sp],Rp,I,Code) ->    
    next([C,B,A|Sp],Rp,I,Code).

rev_rote([A,B,C|Sp],Rp,I,Code) ->    
    next([B,C,A|Sp],Rp,I,Code).

branch(Sp,Rp,I,Code) ->
    next(Sp,Rp,I+element(I,Code),Code).

zbranch([0|Sp],Rp,I,Code) ->
    next(Sp,Rp,I+element(I,Code),Code);
zbranch([_|Sp],Rp,I,Code) ->
    next(Sp,Rp,I+1,Code).

%%

plus([A,B|Sp],Rp,I,Code) ->
    next([B+A|Sp],Rp,I,Code).

star([A,B|Sp],Rp,I,Code) ->
    next([B*A|Sp],Rp,I,Code).

slash([0,_B|_Sp],_Rp,_I,_Code) ->
    exit(badarith);
slash([A,B|Sp],Rp,I,Code) ->
    next([B div A|Sp],Rp,I,Code).

mod([0,_B|_Sp],_Rp,_I,_Code) ->
    exit(badarith);
mod([A,B|Sp],Rp,I,Code) ->
    next([B rem A|Sp],Rp,I,Code).

slash_mod([0,_B|_Sp],_Rp,_I,_Code) ->
    exit(badarith);
slash_mod([A,B|Sp],Rp,I,Code) ->
    next([B rem A,B div A|Sp],Rp,I,Code).

negate([A|Sp],Rp,I,Code) ->
    next([-A|Sp],Rp,I,Code).

over([A,B|Sp],Rp,I,Code) ->
    next([B,A,B|Sp],Rp,I,Code).

drop([_|Sp],Rp,I,Code) ->
    next(Sp,Rp,I,Code).

swap([A,B|Sp],Rp,I,Code) ->
    next([B,A|Sp],Rp,I,Code).

dupe(Sp=[A|_],Rp,I,Code) ->
    next([A|Sp],Rp,I,Code).

abs([A|Sp],Rp,I,Code) ->
    next([abs(A)|Sp],Rp,I,Code).

'and'([A,B|Sp],Rp,I,Code) ->
    next([B band A|Sp],Rp,I,Code).

'or'([A,B|Sp],Rp,I,Code) ->
    next([B bor A|Sp],Rp,I,Code).

invert([A|Sp],Rp,I,Code) ->
    next([bnot A|Sp],Rp,I,Code).

'xor'([A,B|Sp],Rp,I,Code) ->
    next([B bxor A|Sp],Rp,I,Code).

spat(Sp,Rp,I,Code) ->
    next([Sp|Sp], Rp, I, Code).

rpat(Sp,Rp,I,Code) ->
    next([Rp|Sp], Rp, I, Code).

spstore([Sp|_],Rp,I,Code) ->
    next(Sp,Rp,I,Code).

rpstore([Rp|Sp],_Rp,I,Code) ->
    next(Sp,Rp,I,Code).



min([A,B|Sp],Rp,I,Code) ->
    if A < B ->
	    next([A|Sp],Rp,I,Code);
       true ->
	    next([B|Sp],Rp,I,Code)
    end.

max([A,B|Sp],Rp,I,Code) ->
    if A > B ->
	    next([A|Sp],Rp,I,Code);
       true ->
	    next([B|Sp],Rp,I,Code)
    end.
