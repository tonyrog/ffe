-module(ffe).

-export([run/0]).
-export([create/2]).
-compile(export_all).

-define(BYE,       bye).    %% fixme
-define(STACK_OVERFLOW,  -3).
-define(STACK_UNDERFLOW, -4).    %% case clause?
-define(UNDEF,           -13).   %% undefined word
-define(QUIT,            -56).
-define(INTRRUPT,        -28).   %% user interrupt

-record(user,
	{
	  tib = <<>>,  %% text input buffer
	  width = 0,   %% max width of word (fix me not used)
	  dp = {},     %% data pointer (fixme)
	  altin = standard_io,
	  altout = standard_io,
	  in = 0,
	  out = 0,
	  state = 0,
	  forth = forth_words(),  %% forth words
	  current = #{},    %% user defined words
	  base = 10,
	  dpl = 0,
	  csp = [],
	  span = 0,
	  hld = 0,
	  latest = <<>>
	}).

%% State flags
-define(COMPILE,  16#01).
-define(NONAME,   16#02).

%% dictionary word status
-define(SMUDGE,      16#40).
-define(IMMEDIATE,   16#80).

-define(CF_ORIG, 1).
-define(CF_DEST, 2).
-define(CF_DO_SYS, 3).
-define(CF_QDO_SYS, 4).
-define(CF_COLON_SYS, 5).
-define(CF_CASE_SYS, 6).
-define(CF_OF_SYS, 7).
-define(CF_SWITCH_SYS, 8).

run() ->
    init(),
    main([]).

compile(File) ->
    init(),
    {ok,Fd} = file:open(File, []),
    set_user(#user.altin, Fd),
    try main([]) of 
	[] -> 
	    save(File);
	Stack ->
	    io:format("warning: stack element after compilation: ~p\n",
		      [Stack]),
	    save(File)
    catch
	throw:{Code,Reason} ->
	    io:format("error: ~w reason: ~p\n", [Code, Reason]);
	error:Error ->
	    io:format("error: internal error ~p\n", [Error])
    after
	file:close(Fd)
    end.

%% go through dictionary and write them as word defintions.
save(File) ->
    Fd = altout(),
    Module = filename:basename(File, ".fs"),
    io:format(Fd, "-module(~s).\n", [Module]),
    io:format(Fd, "-compile(export_all).\n", []),
    maps:fold(
      fun(Name, Xt, _Acc) ->
	      W = Xt(),
	      io:format("'_ffe_~s'() ->\n  {~w, ~p", 
			[Name,element(1,W),element(2,W)]),
	      print_words(Fd, 3, W),
	      io:format(Fd, "}.\n", [])
      end, [], current()).

print_words(Fd, I, Word) when I =< tuple_size(Word) ->
    io:format(Fd, ",\n  ", []),
    print_word(element(I,Word)),
    print_words(Fd, I+1,Word);
print_words(_Fd, _I, _Word) ->
    ok.


print_word(W) when is_function(W) ->
    Props = erlang:fun_info(W),
    io:format("fun ~s:~s/~w", [proplists:get_value(module,Props),
			       proplists:get_value(name,Props),
			       proplists:get_value(arity,Props)]);
print_word(W) ->
    io:format("~p", [W]).

%% setup test environment
init() ->
    put(user, #user {}),
    ok.

get_user(Field) ->
    element(Field, get(user)).

set_user(Field, Value) ->
    U = setelement(Field, get(user), Value),
    put(user, U),
    Value.

state() -> get_user(#user.state).
state(State) when is_integer(State) -> set_user(#user.state,State).

tib() -> get_user(#user.tib).
tib(Data) when is_binary(Data) -> set_user(#user.tib, Data).
    
span() -> get_user(#user.span).
span(Size) when is_integer(Size),Size>=0 -> set_user(#user.span,Size).

in() -> get_user(#user.in).
in(Offset) when is_integer(Offset),Offset>=0 -> set_user(#user.in, Offset).

out() -> get_user(#user.out).
out(N) when is_integer(N),N>=0 -> set_user(#user.out, N).

%% source is tib in> @
source() -> {tib(),in()}.
source(Data,Offset) -> tib(Data),in(Offset),span(byte_size(Data)).

altin() -> get_user(#user.altin).
altout() -> get_user(#user.altout).

base() -> get_user(#user.base).
base(Base) when is_integer(Base), Base > 1, Base =< 36 ->
    set_user(#user.base, Base).

%% csp is a list of value (stack) in this implementation
csp() -> get_user(#user.csp).    
csp(Stack) -> set_user(#user.csp,Stack).

cf_push(Tag) ->  csp([Tag|csp()]).

cf_pop() ->
    case csp() of
	[] -> throw({-22, control_structure});
	[Tag|Stack1] ->
	    csp(Stack1),
	    Tag
    end.

cf_top() -> hd(csp()).
cf_reset() -> csp([]).

here() -> get_user(#user.dp).
here(Data) when is_tuple(Data) -> set_user(#user.dp, Data).

here_() ->  %% get number of compiled words so far
    tuple_size(here()).

forth() -> get_user(#user.forth).
    
current() -> get_user(#user.current).
current(Dict) -> set_user(#user.current, Dict).

create(Name,W) ->
    io:format("define ~s = ~p = ~p\n", [Name, W, W()]),
    current(maps:put(Name,W,current())).

%% FIRST VERSION in Erlang - to validate the idea
%% REWRITE in forth!
%%
main(SP) ->
    Compile = state() band ?COMPILE =/= 0,
    case word($\s) of
	eof -> 
	    SP;
	Name ->
	    io:format("main: ~s, compile = ~w\n", [Name,Compile]),
	    case find_word_(Name) of
		{false,Xt} when Compile -> comma_(Xt), main(SP);
		{_, Xt} -> exec(Xt(), SP);
		false -> main_number(Compile,Name,SP)
	    end
    end.

%% check for integer/floating point
main_number(Compile,Name,SP) ->
    try binary_to_integer(Name, base()) of
	Int when Compile ->
	    comma_(fun ffe:lit/0),
	    comma_(Int),
	    main(SP);
	Int ->
	    main([Int|SP])
    catch
	error:_ ->
	    try binary_to_float(Name) of
		Flt when Compile ->
		    comma_(fun ffe:lit/0),
		    comma_(Flt),
		    main(SP);
		Flt ->
		    main([Flt|SP])
	    catch
		error:_ ->
		    throw({?UNDEF,Name})
	    end
    end.


exec(W, SP) ->
    CFA = element(3,W),
    try CFA(SP,[],{4,{0,<<"lexec">>,0,fun ret/0}},{4,W}) of
	SP1 -> main(SP1)
    catch
	throw:{?BYE=_Code,_Reason} ->
	    SP;
	throw:{?QUIT=_Code,_Reason} ->
	    main([]);
	throw:{Code,Reason} ->
	    io:format("~w : ~w\n", [Code, Reason]),
	    main([])
    end.

%% [CODE']
%% [']

%% compile only 
code_tick_(Name) ->
    case tick_(Name) of
	{_,Xt} -> element(3, Xt());
	false -> throw({?UNDEF, Name})
    end.

%% lookup a word
tick_(Name) ->
    find_word_(Name).

%%
%% Find a word:
%% return: 
%%      {true, Xt}   -- immediate word
%%      {false,Xt}   -- regualar word
%%      false        -- not recoginsed as a word
%%
find_word_(Name) ->
    case lookup_word_(Name) of
	error ->
	    case binary:split(Name,<<":">>) of
		[Mod,Func] ->
		    M = binary_to_atom(Mod,latin1),
		    F = binary_to_atom(<< <<"_ffe_">>/binary,Func/binary>>,
				       latin1),
		    %% should we check remote calls for immediate ?
		    %% what if module does not exist yet? tricky!
		    {false, fun () -> apply(M, F, []) end};
		_ ->
		    false
	    end;
	{ok,Xt} ->
	    W = Xt(),
	    if element(1,W) band ?IMMEDIATE  =:= ?IMMEDIATE ->
		    {true,Xt};
	       true ->
		    {false,Xt}
	    end
    end.

lookup_word_(Name) ->
    case maps:find(Name, current()) of
	error ->
	    maps:find(Name, forth());
	Found -> Found
    end.

%% TIB  = line buffer
%% SPAN = offset in line buffer
word(Ch) ->
    In = in(),  %% >in @
    case tib() of
	Tib when In >= byte_size(Tib) ->
	    case refill() of
		eof -> eof;
		_Count -> word(Ch)
	    end;
	<<_:In/binary,Data/binary>> ->
	    case enclose(Ch,Data) of
		[_N1,N2,0] ->
		    in(In+N2+1),
		    word(Ch);
		[N1,N2,Len] ->
		    <<_:N1/binary,Word:Len/binary,_/binary>> = Data,
		    in(In+N2+1),
		    Word
	    end
    end.

%% read a line and put it in TIB 
refill() ->
    ALTIN = altin(),
    ALTOUT = altout(),
    case state() of
	STATE when (STATE band ?COMPILE) =:= 0, ALTOUT =/= undefined ->
	    io:format(ALTOUT, "ok ", []);
	_ ->
	    ok
    end,
    case io:get_line(ALTIN, '') of
	eof ->
	    source(<<>>,0),
	    eof;
	Data ->
	    Data1 = erlang:iolist_to_binary(Data),
	    Sz1 = byte_size(Data1)-1,
	    %% strip newline
	    case Data1 of
		<<Data2:Sz1/binary,$\n>> -> source(Data2,0);
		Data2 -> source(Data2,0)
	    end
    end.

%%  ( ch c-addr len -- c-addr n1 n2 n3 )
%%  n1 = offset to first none ch char (word start)
%%  n2 = offset to last char in word (word stop)
%%  n3 = length of enclosed data
enclose(Ch,Data) ->
    Len = byte_size(Data),
    N1 = drop(Ch, Data, 0),
    N2 = take(Ch, Data, N1),
    if N2 < Len -> [N1,N2,(N2-N1)];
       true -> [N1,N2,(N2-N1)]
    end.

%% match characters
match(C, C) -> true;
match($\s, $\t) -> true;
match(_, _) ->  false.

drop(Ch, Data, Offs) ->
    case Data of
	<<_:Offs/binary,C,_/binary>> ->
	    case match(Ch,C) of
		true -> drop(Ch,Data,Offs+1);
		false -> Offs
	    end;
	_ -> Offs
    end.

take(Ch, Data, Offs) ->
    case Data of
	<<_:Offs/binary,C,_/binary>> ->
	    case match(Ch,C) of
		false -> take(Ch,Data,Offs+1);
		true -> Offs
	    end;
	_ ->
	    Offs
    end.

is_ffe_func(M,Func) when is_binary(Func) ->
    F = binary_to_atom(<< <<"_ffe_">>/binary,Func/binary>>,latin1),
    %% ensure loded?
    case erlang:function_exported(M,F,4) of
	true  -> {true,F};
	false -> {false,binary_to_atom(Func,latin1)}
    end.

find_first_arity(M,F) ->
    Fs = [X||X={Fm,_} <- M:module_info(exports),Fm =:= F],
    case lists:sort(Fs) of
	[] -> 1;
	[{_,A}|_] -> A
    end.
    
-define(PRIM(Name,Call), <<Name>> => fun ffe:Call/0).

%% "primitive" forth words, we may compile some of them soon
forth_words() ->
    #{
       ?PRIM(":",          colon),
       ?PRIM(";",          semicolon),
       ?PRIM("constant",   constant),
       ?PRIM("user",       user),

       ?PRIM("(semis)",    semis),
       ?PRIM("(docol)",    docol),
       ?PRIM("(douser)",   dousr),
       ?PRIM("do",         do),
       ?PRIM("?do",        qdo),
       ?PRIM("loop",       loop),
       ?PRIM("\\",         backslash),
       ?PRIM("(",          paren),
       ?PRIM("[",          left_bracket),
       ?PRIM("]",          right_bracket),
       ?PRIM("(do)",       pdo),
       ?PRIM("(?do)",      pqdo),
       ?PRIM("(loop)",     ploop),
       ?PRIM("@",          fetch),
       ?PRIM("!",          store),
       ?PRIM("sp@",        spat),
       ?PRIM("rp@",        rpat),
       ?PRIM("sp!",        spstore),
       ?PRIM("rp!",        rpstore),
       ?PRIM("i",          i),
       ?PRIM("j",          j),
       ?PRIM("leave",      leave),
       ?PRIM(".",          dot),
       ?PRIM("quit",       quit),
       ?PRIM("bye",        bye),
       ?PRIM("noop",       noop),
       ?PRIM("literal",    lit),
       ?PRIM("rot",        rote),
       ?PRIM("-rot",       rev_rote),
       ?PRIM("branch",     branch),
       ?PRIM("0branch",    zbranch),
       ?PRIM("+",          plus),
       ?PRIM("-",          minus),
       ?PRIM("1-",         one_minus),
       ?PRIM("1+",         one_plus),
       ?PRIM("*",          star),
       ?PRIM("/",          slash),
       ?PRIM("mod",        mod),
       ?PRIM("/mod",       slash_mod),
       ?PRIM("negate",     negate),
       ?PRIM("over",       over),
       ?PRIM("drop",       drop),
       ?PRIM("swap",       swap),
       ?PRIM("dup",        dupe),
       ?PRIM("abs",        abs),
       ?PRIM("and",        'and'),
       ?PRIM("invert",     'invert'),
       ?PRIM("or",         'or'),
       ?PRIM("xor",        'xor'),
       ?PRIM("min",        min),
       ?PRIM("max",        max)
     }.

%%
%% General word layout
%% {Flags, <<"Name">>, fun cfa/4, fun pf1/0, ... fun pfn/0}
%%
next(SP,RP,{IP,Code},{_WP,_W}) ->
    WF = element(IP, Code),  %% function returning tuple def
    W = WF(),                %% tuple word
    io:format("next: ~s (~w) stack: ~p\n", [element(2,W),WF,SP]),  %% print NFA
    CFA = element(3,W),      %% CFA is located
    CFA(SP,RP,{IP+1,Code},{4,W}).

%% colon definition
colon() ->
    { ?IMMEDIATE, <<":">>, fun colon/4 }.
colon(SP, RP, IP, WP) ->
    interpreting(),
    cf_reset(),
    state(?COMPILE),
    Name = word($\s),
    here({0,Name,fun ffe:docol/4}),
    next(SP, RP, IP, WP).

smudge() ->
    { 0, <<"smudge">>, fun smudge/4 }.
smudge(SP, RP, IP, WP) ->
    %% different than standard
    Def = here(),
    Def1 = setelement(1, Def, element(1, Def) bxor ?SMUDGE),
    here(Def1),
    next(SP, RP, IP, WP).

semicolon() ->
    { ?IMMEDIATE, <<";">>, fun semicolon/4 }.
semicolon(SP,RP,IP,WP) ->
    compile_only(),
    case csp() of
	[] ->
	    Def = comma_(fun ffe:semis/0),
	    Name = element(2, Def),
	    create(Name, fun() -> Def end),
	    here({}),  %% clear defintion area
	    state(0),
	    next(SP,RP,IP,WP);
	_ ->
	    throw({-22, control_structure})
    end.

constant() ->
    { 0, <<"constant">>, fun constant/4 }.
constant([Value|SP],RP,IP,WP) ->
    Name = word($\s),
    create(Name, fun() -> {0, Name, fun docon/4, Value } end),
    next(SP,RP,IP,WP).

user() ->
    { 0, <<"user">>, fun user/4 }.
user([Value|SP],RP,IP,WP) ->
    Name = word($\s),
    create(Name, fun() -> {0, Name, fun ffe:dousr/4, Value } end),
    next(SP,RP,IP,WP).
    

%% immediate word
do() ->
    {?IMMEDIATE, <<"do">>, fun do/4 }.
do(SP,RP,IP,WP) ->
    compile_only(),
    cf_push(here_()), cf_push(?CF_DO_SYS),
    comma_(fun ffe:pdo/0),
    next(SP,RP,IP,WP).

%% immediate word
qdo() ->
    {?IMMEDIATE, <<"?do">>, fun qdo/4 }.
qdo(SP,RP,IP,WP) ->
    compile_only(),
    cf_push(here_()), cf_push(?CF_QDO_SYS),
    comma_(fun ffe:pqdo/0),
    comma_(0),  %% patch this place
    next(SP,RP,IP,WP).

%% immediate word
loop() ->
    {?IMMEDIATE, <<"loop">>, fun loop/4 }.
loop(SP,RP,IP,WP) ->
    compile_only(),
    case cf_pop() of
	?CF_DO_SYS ->
	    comma_(fun ffe:ploop/0),
	    Pos = cf_pop(),
	    back_(Pos+1);
	?CF_QDO_SYS ->
	    comma_(fun ffe:ploop/0),
	    Pos = cf_pop(),
	    back_(Pos+2),
	    forward_patch_(Pos+2);
	_ -> 
	    throw({-22, "loop missing DO/?DO"})
    end,
    next(SP,RP,IP,WP).

comma() ->
    {0, <<",">>, fun comma/4 }.
comma([Value|SP],RP,IP,WP) ->
    comma_(Value),
    next(SP,RP,IP,WP).

comma_(Value) ->
    Here = here(),
    here(erlang:append_element(Here, Value)).

back_(Pos) ->
    NextPos = here_(),
    Offset = Pos - NextPos,
    comma_(Offset).

forward_patch_(Pos) ->
    NextPos = here_(),
    Offset = NextPos - Pos + 1,
    Here = here(),
    here(erlang:setelement(Pos,Here,Offset)).
    
pdo() ->
    { 0, <<"(do)">>, fun pdo/4 }.
pdo([I,N|SP],RP,IP,WP) ->
    next(SP,[I,N|RP],IP,WP).

%% ?DO check condition before the loop
pqdo() ->
    { 0, <<"(?do)">>, fun pqdo/4 }.
pqdo([I,N|SP],RP,{IP,Code},WP) when I < N ->
    next(SP,[I,N|RP],{IP+1,Code},WP);
pqdo([_I,_N|SP],RP,{IP,Code},WP) ->
    next(SP,RP,{IP+element(IP,Code),Code},WP).

ploop() ->
    {0, <<"(loop)">>, fun ploop/4 }.
ploop(SP,[I|RP=[N|RP1]],{IP,Code},WP) ->
    %% io:format("ploop: i=~w, n=~w\n", [I,N]),
    if I+1 < N ->
	    next(SP,[I+1|RP],{IP+element(IP,Code),Code},WP);
       true ->
	    next(SP,RP1,{IP+1,Code},WP)
    end.

i() ->
    {0, <<"i">>, fun i/4}.
i(SP,[Ix|_]=RP,IP,Code) ->
    next([Ix|SP],RP,IP,Code).

j() ->
    {0, <<"j">>, fun j/4}.
j(SP,[_,_,Jx|_]=RP,IP,Code) ->
    next([Jx|SP],RP,IP,Code).

fetch() ->
    {0, <<"@">>, fun fetch/4 }.
fetch([Addr|SP],RP,IP,Code) ->
    case Addr of
	{user,Field} ->
	    Value = case get_user(Field) of
			undefined -> 0;
			V -> V
		    end,
	    next([Value|SP],RP,IP,Code);
	{sys,_} ->
	    Value = case ets:lookup(forth,Addr) of
			[] -> 0;
			[{_,V}] -> V
		    end,
	    next([Value|SP],RP,IP,Code)
    end.

store() ->
    {0, <<"!">>, fun store/4 }.
store([Addr,Value|SP],RP,IP,Code) ->
    case Addr of
	{user,_} -> 
	    set_user(Addr,Value),
	    next(SP,RP,IP,Code);
	{sys,_} ->
	    ets:insert(forth,{Addr,Value}),
	    next(SP,RP,IP,Code)
    end.

leave() ->
    {0, <<"leave">>, fun leave/4 }.
leave(SP,[_,RP=[Limit|_]],IP,WP) ->
    next(SP,[Limit|RP],IP,WP).

quit() ->
    {0, <<"quit">>, fun quit/4 }.
quit(_SP,_RP,_IP,_WP) ->
    throw({?QUIT, quit}).

bye() ->
    {0, <<"bye">>, fun bye/4 }.
bye(_SP,_RP,_IP,_WP) ->
    throw({?BYE, exit}).

ret() ->
    { 0, <<"ret">>, fun ret/4 }.
ret(SP,_RP,_IP,_Code) ->
    SP.

noop() ->
    { 0, <<"noop">>, fun noop/4 }.
noop(SP,RP,I,Code) ->
    next(SP,RP,I,Code).

lit() ->
    { 0, "lit", fun lit/4 }.
lit(SP,RP,{IP,Code},WP) ->
    next([element(IP,Code)|SP],RP,{IP+1,Code},WP).

execut() ->
    { 0, "execut", fun execut/4 }.
execut([WP|SP],RP,IP,_WP) ->
    W = WP(),            %% tuple word
    CFA = element(3,W),  %% CFA is located
    CFA(SP,RP,IP,{4,W}).

dodoes(SP,RP,IP,WP) ->
    docol([WP|SP],RP,IP,WP). %% fixme.


docol() ->
    { 0, "(docol)", fun docol/4 }.
docol(SP,RP,IP,WP) ->
    next(SP,[IP|RP],WP,WP).

docon() ->
    { 0, "(docon)", fun docon/4 }.
docon(SP,RP,IP,WP0={W,WP}) ->
    next([element(W,WP)|SP],RP,IP,WP0).

dousr() ->
    { 0, "(dousr)", fun dousr/4 }.
dousr(SP,RP,IP,WP0={W,WP}) ->
    next([{user,element(W,WP)}|SP],RP,IP,WP0).

does(SP,RP,IP,WP) ->
    next([WP|SP],RP,IP,WP).

semis() ->    
    { 0, "(semis)", fun semis/4 }.
semis(SP,[IP|RP],_IP,WP) ->
    next(SP,RP,IP,WP).

branch() ->
    {0, <<"branch">>, fun branch/4 }.
branch(SP,RP,{IP,Code},WP) ->
    next(SP,RP,{IP+element(IP,Code),Code},WP).

zbranch() ->
    {0, <<"0branch">>, fun zbranch/4 }.
zbranch([0|SP],RP,{IP,Code},WP) ->
    next(SP,RP,{IP+element(IP,Code)},WP);
zbranch([_|SP],RP,{IP,Code},WP) ->
    next(SP,RP,{IP+1,Code},WP).

%%
dot() ->
    {0, <<".">>, fun dot/4 }.
dot([Value|SP],RP,IP,WP) ->
    if is_integer(Value) ->
	    erlang:display_string(integer_to_list(Value, base()));
       true -> %% hmm recursivly display integers in base()!
	    erlang:display_string(lists:flatten(io_lib:format("~p",[Value])))
    end,
    next(SP,RP,IP,WP).

%% words manuplating stack only

rote() ->
    {0, <<"rot">>, fun rote/4}.
rote([A,B,C|SP],RP,I,Code) ->    
    next([C,B,A|SP],RP,I,Code).

rev_rote() ->
    {0, <<"-rot">>, fun rev_rote/4}.
rev_rote([A,B,C|SP],RP,IP,Code) ->
    next([B,C,A|SP],RP,IP,Code).

plus() ->
    { 0, <<"+">>, fun plus/4 }.
plus([A,B|SP],RP,IP,WP) ->
    next([B+A|SP],RP,IP,WP).

one_plus() ->
    { 0, <<"1+">>, fun one_plus/4 }.
one_plus([A|SP],RP,IP,WP) ->
    next([A+1|SP],RP,IP,WP).

minus() ->
    { 0, <<"-">>, fun minus/4 }.
minus([A,B|SP],RP,IP,WP) ->
    next([B-A|SP],RP,IP,WP).

one_minus() ->
    { 0, <<"1-">>, fun one_minus/4 }.
one_minus([A|SP],RP,IP,WP) ->
    next([A-1|SP],RP,IP,WP).

star() ->
    { 0, <<"*">>, fun star/4 }.
star([A,B|SP],RP,IP,WP) ->
    next([B*A|SP],RP,IP,WP).

slash() ->
    { 0, <<"/">>, fun slash/4 }.    
slash([0,_B|_SP],_RP,_IP,_WP) ->
    exit(badarith);
slash([A,B|SP],RP,IP,WP) ->
    next([B div A|SP],RP,IP,WP).

mod() ->
    { 0, <<"mod">>, fun mod/4 }.
mod([0,_B|_SP],_RP,_IP,_WP) ->
    exit(badarith);
mod([A,B|SP],RP,IP,WP) ->
    next([B rem A|SP],RP,IP,WP).

slash_mod() ->
    { 0, <<"/mod">>, fun slash_mod/4 }.
slash_mod([0,_B|_SP],_RP,_IP,_WP) ->
    exit(badarith);
slash_mod([A,B|SP],RP,IP,WP) ->
    next([B rem A,B div A|SP],RP,IP,WP).

negate() ->
    { 0, <<"negate">>, fun negate/4 }.
negate([A|SP],RP,IP,WP) ->
    next([-A|SP],RP,IP,WP).

over() ->
    { 0, <<"over">>, fun over/4 }.
over([A,B|SP],RP,IP,WP) ->
    next([B,A,B|SP],RP,IP,WP).

drop() ->
    { 0, <<"drop">>, fun drop/4 }.
drop([_|SP],RP,IP,WP) ->
    next(SP,RP,IP,WP).

swap() ->
    { 0, <<"swap">>, fun swap/4 }.
swap([A,B|SP],RP,IP,WP) ->
    next([B,A|SP],RP,IP,WP).

dupe() ->
    { 0, <<"dup">>, fun dupe/4 }.
dupe(SP=[A|_],RP,IP,WP) ->
    next([A|SP],RP,IP,WP).

abs() ->
    { 0, <<"abs">>, fun abs/4 }.
abs([A|SP],RP,IP,WP) ->
    next([abs(A)|SP],RP,IP,WP).

'and'() ->
    { 0, <<"and">>, fun 'and'/4 }.
'and'([A,B|SP],RP,IP,WP) ->
    next([B band A|SP],RP,IP,WP).

'or'() ->
    { 0, <<"or">>, fun 'or'/4 }.
'or'([A,B|SP],RP,IP,WP) ->
    next([B bor A|SP],RP,IP,WP).

invert() ->
    { 0, <<"invert">>, fun invert/4 }.
invert([A|SP],RP,IP,WP) ->
    next([bnot A|SP],RP,IP,WP).

'xor'() ->
    { 0, <<"xor">>, fun 'xor'/4 }.
'xor'([A,B|SP],RP,IP,WP) ->
    next([B bxor A|SP],RP,IP,WP).

spat() ->
    { 0, <<"sp@">>, fun spat/4 }.
spat(SP,RP,IP,WP) ->
    next([SP|SP],RP,IP,WP).

rpat() ->
    { 0, <<"rp@">>, fun rpat/4 }.
rpat(SP,RP,IP,WP) ->
    next([RP|SP],RP,IP,WP).

spstore() ->
    { 0, <<"sp!">>, fun spstore/4 }.
spstore([SP|_],RP,IP,WP) ->
    next(SP,RP,IP,WP).

rpstore() ->
    { 0, <<"rp!">>, fun rpstore/4 }.
rpstore([RP|SP],_RP,IP,WP) ->
    next(SP,RP,IP,WP).

min() ->
    { 0, <<"min">>, fun min/4 }.
min([A,B|SP],RP,IP,WP) ->
    if A < B -> next([A|SP],RP,IP,WP);
       true -> next([B|SP],RP,IP,WP)
    end.

max() ->
    { 0, <<"max">>, fun max/4 }.
max([A,B|SP],RP,IP,WP) ->
    if A > B -> next([A|SP],RP,IP,WP);
       true -> next([B|SP],RP,IP,WP)
    end.

%% IMMEDIATE
backslash() ->
    { ?IMMEDIATE, <<"\\">>, fun backslash/4 }.
backslash(SP,RP,IP,WP) ->
    in(span()),  %% skip all characters in input buffer
    next(SP,RP,IP,WP).

paren() -> 
    { ?IMMEDIATE, <<"(">>, fun paren/4 }. 
paren(SP,RP,IP,WP) ->
    word($)),       %% skip until ')'
    next(SP,RP,IP,WP).

right_bracket() ->
    { ?IMMEDIATE, <<"]">>, fun right_bracket/4 }. 
right_bracket(SP,RP,IP,WP) ->
    state(state() bor ?COMPILE),
    next(SP,RP,IP,WP).

left_bracket() ->
    { ?IMMEDIATE, <<"[">>, fun right_bracket/4 }. 
left_bracket(SP,RP,IP,WP) ->
    state(state() band (bnot ?COMPILE)),
    next(SP,RP,IP,WP).

%% utils - fixme define in forth
compile_only() ->
    case state() of
	STATE when STATE band ?COMPILE =:= 0 -> throw({-14,compile_only});
	_ -> ok
    end.

interpreting() ->
    case state() of
	0 -> ok;
	_ -> throw({-29, compiler_nesting})
    end.
