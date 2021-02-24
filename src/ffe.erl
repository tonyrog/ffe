-module(ffe).

-export([start/0]).
-export([run/0]).
-export([define/2]).
-compile(export_all).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

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

-define(CF_CREATE, 100).

-include("ffe.hrl").

start() ->
    init(),
    main([]).

run() -> start().

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
	?EXCEPTION(error,Error,Trace) ->
	    io:format("error: internal error ~p\n~p", 
		      [Error, ?GET_STACK(Trace)])
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
    io:format("fun '~s':'~s'/~w", [proplists:get_value(module,Props),
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

get_state() -> get_user(#user.state).
set_state(State) when is_integer(State) -> set_user(#user.state,State).

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
here(Data) when is_tuple(Data) -> 
    %% io:format("set here = ~p\n", [Data]),
    set_user(#user.dp, Data).

here_() ->  %% get number of compiled words so far
    tuple_size(here()).

forth() -> get_user(#user.forth).
    
current() -> get_user(#user.current).
current(Dict) -> set_user(#user.current, Dict).

%% Add Name to current vocabulary
define(Name,W) ->
    io:format("define ~s = ~p = ~p\n", [Name, W, W()]),
    current(maps:put(Name,W,current())).

%% FIRST VERSION in Erlang - to validate the idea
%% REWRITE in forth!
%%
quit() ->
    tib(<<>>), in(0),
    main([]).

main(SP) ->
    Compile = is_compiling(),
    case word($\s) of
	eof ->
	    SP;
	Name ->
	    %% io:format("main: ~s, compile = ~w\n", [Name,Compile]),
	    case find_word_(Name) of
		{false,Xt} when Compile -> comma_(Xt), main(SP);
		{_, Xt} -> exec(Xt(), SP);
		false -> main_number(Compile,Name,SP)
	    end
    end.

%% check for integer/floating point
main_number(Compile,Name,SP) ->
    try to_integer(Name, base()) of
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
		    io:format("~w, undef : ~s\n", [?UNDEF, Name]),
		    quit()
	    end
    end.

%% $abc ignore base and use hex number
to_integer(<<$$,Cs/binary>>, _Base) ->
    binary_to_integer(Cs, 16);
to_integer(Cs, Base) ->
    binary_to_integer(Cs, Base).

exec(W, SP) ->
    CFA = element(3,W),
    try CFA(SP,[],{4,{0,<<"lexec">>,0,fun ret/0}},{4,W}) of
	SP1 -> main(SP1)
    catch
	throw:{?BYE=_Code,_Reason} ->
	    halt(0); %% ??
	throw:{?QUIT=_Code,_Reason} ->
	    quit();
	throw:{?UNDEF=Code,Reason} ->
	    io:format("~w : ~p\n", [Code, Reason]),
	    quit();
	throw:{Code,Reason} ->
	    io:format("~w : ~w\n", [Code, Reason]),
	    quit();
	?EXCEPTION(error,{case_clause,SP0},_StackTrace) when is_list(SP0) ->
	    CallStack = ?GET_STACK(_StackTrace),
	    Calls = string:join(callstack(CallStack), " "),
	    %% fixme: print forth word where error occured 
	    io:format("error: stack under flow\n", []),
	    io:format("  call: ~p\n", [Calls]),
	    io:format("  call stack: ~p\n", [CallStack]),
	    quit();
	?EXCEPTION(error,function_clause,_StackTrace) ->
	    CallStack = ?GET_STACK(_StackTrace),
	    Calls = string:join(callstack(CallStack), " "),
	    %% fixme: print forth word where error occured 
	    io:format("error: stack under flow\n", []),
	    io:format("  call: ~p\n", [Calls]),
	    io:format("  call stack: ~p\n", [CallStack]),
	    quit();

	?EXCEPTION(error,Reason,_StackTrace) ->
	    CallStack = ?GET_STACK(_StackTrace),
	    io:format("error: internal error: ~p\n", [Reason]),
	    io:format("  call stack: ~p\n", [CallStack]),	    
	    quit()
    end.

callstack([{ffe,Word,_,_}|Stack]) ->
    [atom_to_list(Word) | callstack(Stack)];
callstack([_ | _Stack]) ->
    ["..."];
callstack([]) ->
    [].

%% [CODE']
%% [']

bracket_tick() ->
    { ?IMMEDIATE, <<"[']">>, fun bracket_tick_/4 }.
%% compile only 
bracket_tick_(SP,RP,IP,WP) ->
    compile_only(),
    Name = word($\s),
    case tick_(Name) of
	{_,Xt} -> 
	    comma_(Xt),
	    next(SP, RP, IP, WP);
	false -> 
	    throw({?UNDEF, Name})
    end.

tick() ->
    { 0, <<"'">>, fun tick_/4 }.
%% lookup a word
tick_(SP, RP, IP, WP) ->
    Name = word($\s),
    case tick_(Name) of
	{_,Xt} -> 
	    next([Xt|SP], RP,IP, WP);
	false -> 
	    throw({?UNDEF, Name})
    end.

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
		    %% fun () -> apply(M, F, []) end
		    {false, erlang:make_fun(M,F,0)};
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
    case is_compiling() of
	false when ALTOUT =/= undefined ->
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

find_first_arity(M,F) ->
    Fs = [X||X={Fm,_} <- M:module_info(exports),Fm =:= F],
    case lists:sort(Fs) of
	[] -> 1;
	[{_,A}|_] -> A
    end.
    
-define(WORD(Name,Call), <<Name>> => fun ffe:Call/0).

%% "primitive" forth words, we may compile some of them soon
%%  this "dictionary" should/must? be constant
forth_words() ->
    #{
      ?WORD("(docol)",    docol),
      ?WORD("(dousr)",    dousr),
      %% ?WORD("(dovoc)",    dovoc),
      ?WORD("(does)",     pdoes),
      ?WORD("(does@)",    does_fetch),
      ?WORD("(dovar)",    dovar),
      ?WORD("(dovar@)",   dovar_fetch),
      ?WORD("(doval)",    doval),
      ?WORD("(doval@)",   doval_fetch),
      ?WORD("(dodoes)",   dodoes),
      ?WORD("(execute)",  execute),
      %% (does-code) doescode),
      ?WORD("execute",    execut),
      ?WORD("branch",     branch),
      ?WORD("0branch",    zbranch),
      
      ?WORD("(semis)",    semis),
      
      ?WORD("(do)",       pdo),
      ?WORD("(?do)",      pqdo),
      ?WORD("(loop)",     ploop),
      ?WORD("noop",       '_ffe_noop'),
      
      ?WORD("@",          '_ffe_fetch'),
      ?WORD("!",          '_ffe_store'),
      ?WORD("sp@",        '_ffe_spat'),
      ?WORD("rp@",        '_ffe_rpat'),
      ?WORD("sp!",        '_ffe_spstore'),
      ?WORD("rp!",        '_ffe_rpstore'),
      ?WORD("i",          '_ffe_i'),
      ?WORD("j",          '_ffe_j'),
      ?WORD("leave",      '_ffe_leave'),
      ?WORD(">r",         '_ffe_tor'),
      ?WORD("r>",         '_ffe_rfrom'),
      
      ?WORD("rot",        '_ffe_rote'),
      ?WORD("-rot",       '_ffe_rev_rote'),
      
      ?WORD("+",          '_ffe_plus'),
      ?WORD("-",          '_ffe_minus'),
      ?WORD("1-",         '_ffe_one_minus'),
      ?WORD("1+",         '_ffe_one_plus'),
      ?WORD("*",          '_ffe_star'),
      ?WORD("/",          '_ffe_slash'),
      ?WORD("mod",        '_ffe_mod'),
      ?WORD("/mod",       '_ffe_slash_mod'),
      ?WORD("negate",     '_ffe_negate'),
      ?WORD("over",       '_ffe_over'),
      ?WORD("drop",       '_ffe_drop'),
      ?WORD("swap",       '_ffe_swap'),
      ?WORD("dup",        '_ffe_dupe'),
      ?WORD("lshift",     '_ffe_lshift'),
      ?WORD("rshift",     '_ffe_rshift'),
      ?WORD("arshift",     '_ffe_arshift'),
      ?WORD("and",        '_ffe_and'),
      ?WORD("invert",     '_ffe_invert'),
      ?WORD("or",         '_ffe_or'),
      ?WORD("xor",        '_ffe_xor'),
      ?WORD("abs",        '_ffe_abs'),
      ?WORD("min",        '_ffe_min'),
      ?WORD("max",        '_ffe_max'),
      ?WORD("0=",         '_ffe_zero_equals'),
      ?WORD("0<",         '_ffe_zero_less'),
      ?WORD("0",          '_ffe_zero'),
      ?WORD("1",          '_ffe_one'),
      ?WORD("-1",         '_ffe_minus_one'),
      
      %% system 
      ?WORD("quit",       '_ffe_quit'),
      ?WORD("bye",        '_ffe_bye'),
      
      ?WORD("CREATE",     '_ffe_create'),
      ?WORD("DOES>",      '_ffe_does'),
      
      %% meta words - pre compile
      ?WORD(":",          colon),
      ?WORD(";",          semicolon),
      ?WORD("CONSTANT",   compile_constant),
      ?WORD("USER",       compile_user),
      ?WORD("DO",         compile_do),
      ?WORD("?DO",        compile_qdo),
      ?WORD("LOOP",       compile_loop),
      ?WORD("IF",         compile_if),
      ?WORD("THEN",       compile_then),
      ?WORD("ELSE",       compile_else),
      ?WORD(",",          comma),
      
      ?WORD("\\",         backslash),
      ?WORD("(",          paren),
      ?WORD("[",          left_bracket),
      ?WORD("]",          right_bracket),
      ?WORD(".",          '_ffe_dot'),
      ?WORD("LITERAL",    lit),
      ?WORD("\"",         string),
      ?WORD("'",          tick),
      ?WORD("[']",        bracket_tick)
     }.

%%
%% General word layout
%% {Flags, <<"Name">>, fun cfa/4, fun pf1/0, ... fun pfn/0}
%%
next(SP,RP,{IP,Code},{_WP,_W}) ->
    WF = element(IP, Code),  %% function returning tuple def
    %% io:format("WF = ~p\n", [WF]),
    W = WF(),                %% tuple word
    %% io:format("next: ~s (~w) stack: ~p\n", [element(2,W),WF,SP]),
    CFA = element(3,W),      %% CFA is located
    CFA(SP,RP,{IP+1,Code},{4,W}).

%% colon definition
colon() ->
    { ?IMMEDIATE, <<":">>, fun colon/4 }.
colon(SP, RP, IP, WP) ->
    interpreting(),
    cf_reset(),
    set_state(?COMPILE),
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
	    define(Name, fun() -> Def end),
	    here({}),  %% clear defintion area
	    set_state(0),
	    next(SP,RP,IP,WP);
	_ ->
	    throw({-22, control_structure})
    end.

%% CONSTANT - ffe compiler
compile_constant() ->
    { 0, <<"CONSTANT">>, fun compile_constant/4 }.
compile_constant([Value|SP],RP,IP,WP) ->
    Name = word($\s),
    define(Name, fun() -> {0, Name, fun docon/4, Value } end),
    next(SP,RP,IP,WP).

%% USER - ffe compiler
compile_user() ->
    { 0, <<"USER">>, fun compile_user/4 }.
compile_user([Value|SP],RP,IP,WP) ->
    Name = word($\s),
    define(Name, fun() -> {0, Name, fun ffe:dousr/4, Value } end),
    next(SP,RP,IP,WP).

%% , is currently a bit special
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

forward_patch__(Pos) ->
    NextPos = here_(),
    Offset = NextPos - Pos + 1,
    Here = here(),
    here(erlang:setelement(Pos+1,Here,Offset-1)).
    

%% DO - ffe compiler, immediate word
compile_do() ->
    {?IMMEDIATE, <<"DO">>, fun compile_do/4 }.
compile_do(SP,RP,IP,WP) ->
    compile_only(),
    cf_push(here_()), cf_push(?CF_DO_SYS),
    comma_(fun ffe:pdo/0),
    next(SP,RP,IP,WP).

%% ?DO immediate word
compile_qdo() ->
    {?IMMEDIATE, <<"?DO">>, fun compile_qdo/4 }.
compile_qdo(SP,RP,IP,WP) ->
    compile_only(),
    cf_push(here_()), cf_push(?CF_QDO_SYS),
    comma_(fun ffe:pqdo/0),
    comma_(0),  %% patch this place
    next(SP,RP,IP,WP).

%% LOOP immediate word
compile_loop() ->
    {?IMMEDIATE, <<"LOOP">>, fun compile_loop/4 }.
compile_loop(SP,RP,IP,WP) ->
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

compile_if() ->
    { ?IMMEDIATE, <<"IF">>, fun compile_if/4}.
compile_if(SP,RP,IP,WP) ->
    compile_only(),
    comma_(fun ffe:zbranch/0),    %% postpone 0branch
    cf_push(here_()), cf_push(?CF_ORIG),
    comma_(0),  %% patch this place
    next(SP,RP,IP,WP).

compile_then() ->
    { ?IMMEDIATE, <<"THEN">>, fun compile_then/4}.
compile_then(SP,RP,IP,WP) ->
    compile_only(),
    case cf_pop() of
	?CF_ORIG ->
	    Pos = cf_pop(),
	    forward_patch__(Pos),
	    next(SP,RP,IP,WP);
	_ ->
	    throw({-22, "THEN missing IF/ELSE"})
    end.


compile_else() ->
    { ?IMMEDIATE, <<"ELSE">>, fun compile_else/4}.
compile_else(SP,RP,IP,WP) ->
    compile_only(),
    case cf_pop() of
	?CF_ORIG ->
	    Pos = cf_pop(),
	    comma_(fun ffe:branch/0),    %% postpone 0branch
	    cf_push(here_()), cf_push(?CF_ORIG),
	    comma_(0),  %% patch this place
	    forward_patch__(Pos),
	    next(SP,RP,IP,WP);
	_ ->
	    throw({-22, "ELSE missing IF"})
    end.

'_ffe_create'() ->
    {0, <<"CREATE">>, fun create/4}.
create(SP,RP,IP,WP) ->
    Name = word($\s),
    io:format("create word ~s\n", [Name]),
    here({0,Name,fun ffe:does0/4, 0}),
    cf_push(?CF_CREATE),
    next(SP, RP, IP, WP).

'_ffe_does'() ->
    { 0, <<"DOES>">>, fun does/4 }.
does(SP,RP,IP,WP) ->
    case csp() of
	[?CF_CREATE|Csp] -> %% only in create?
	    %% use this position as landing point for the word beeing defined
	    Def0 = here(),
	    Name = element(2, Def0),
	    Def1 = setelement(3, Def0, fun ffe:does1/4),
	    {DoesPos,DoesCode} = IP,
	    DoesName = element(2, DoesCode),
	    {_, DoesXt} = find_word_(DoesName),
	    Def2 = setelement(4, Def1, {DoesPos,DoesXt}),
	    define(Name, fun() -> Def2 end),
	    here({}),  %% clear defintion area
	    csp(Csp),  %% pop control stack
	    [IP1|RP1] = RP,
	    next(SP,RP1,IP1,WP)
    end.
    
pdo() ->
    { 0, <<"(do)">>, fun pdo/4 }.
pdo(_SP0=[I,N|SP],RP,IP,WP) ->
    io:format("pdo: ~p\n", [_SP0]),
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
    io:format("ploop: i=~w, n=~w, stack=~w\n", [I,N,SP]),
    if I < N-2 ->
	    next(SP,[I+1|RP],{IP+element(IP,Code),Code},WP);
       true ->
	    next(SP,RP1,{IP+1,Code},WP)
    end.

'_ffe_i'() ->
    {0, <<"i">>, fun i/4}.
i(SP,[Ix|_]=RP,IP,Code) ->
    next([Ix|SP],RP,IP,Code).

'_ffe_j'() ->
    {0, <<"j">>, fun j/4}.
j(SP,[_,_,Jx|_]=RP,IP,Code) ->
    next([Jx|SP],RP,IP,Code).

'_ffe_fetch'() ->
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
	    next([Value|SP],RP,IP,Code);

	{Pos,Cells} when is_integer(Pos), is_tuple(Cells) ->
	    next([element(Pos,Cells)|SP],RP,IP,Code)
    end.

'_ffe_store'() ->
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

'_ffe_leave'() ->
    {0, <<"leave">>, fun leave/4 }.
leave(SP,[_,RP=[Limit|_]],IP,WP) ->
    next(SP,[Limit|RP],IP,WP).

'_ffe_tor'() ->
    {0, <<">r">>, fun tor/4 }.
tor([E|SP],RP,IP,WP) ->
    next(SP,[E|RP],IP,WP).

'_ffe_rfrom'() ->
    {0, <<"r>">>, fun rfrom/4 }.
rfrom(SP,[E|RP],IP,WP) ->
    next([E|SP],RP,IP,WP).

'_ffe_quit'() ->
    {0, <<"quit">>, fun quit/4 }.
quit(_SP,_RP,_IP,_WP) ->
    throw({?QUIT, quit}).

'_ffe_bye'() ->
    {0, <<"bye">>, fun bye/4 }.
bye(_SP,_RP,_IP,_WP) ->
    throw({?BYE, exit}).

ret() ->
    { 0, <<"ret">>, fun ret/4 }.
ret(SP,_RP,_IP,_Code) ->
    SP.

'_ffe_noop'() ->
    { 0, <<"noop">>, fun noop/4 }.
noop(SP,RP,I,Code) ->
    next(SP,RP,I,Code).

lit() ->
    { 0, <<"lit">>, fun lit/4 }.
lit(SP,RP,{IP,Code},WP) ->
    next([element(IP,Code)|SP],RP,{IP+1,Code},WP).

execut() ->
    { 0, <<"execut">>, fun execut/4 }.
execut([WP|SP],RP,IP,_WP) ->
    W = WP(),            %% tuple word
    CFA = element(3,W),  %% CFA is located
    CFA(SP,RP,IP,{4,W}).

%% WP points to current executing word, move that to point to the
%% new word PFA area
dodoes(SP,RP,IP,WP={Wi,W}) ->
    W1 = element(Wi-1,W()),
    WP1 = {4,W1},
    docol([WP|SP],RP,IP,WP1).

docol() ->
    { 0, <<"(docol)">>, fun docol/4 }.
docol(SP,RP,IP,WP) ->
    next(SP,[IP|RP],WP,WP).

docon() ->
    { 0, <<"(docon)">>, fun docon/4 }.
docon(SP,RP,IP,WP0={W,WP}) ->
    %% io:format("do con WP0 = ~w\n", [WP0]),
    next([element(W,WP)|SP],RP,IP,WP0).

dousr() ->
    { 0, "(dousr)", fun dousr/4 }.
dousr(SP,RP,IP,WP0={W,WP}) ->
    next([{user,element(W,WP)}|SP],RP,IP,WP0).

%% default does code with no offset
does0(SP,RP,{IP,Code},WP={Pos,Wf}) ->
    next([{Pos+1,Wf}|SP],RP,{IP+1,Code},WP).

%% does with offset to does> code
does1() ->
    { 0, <<"(does>)">>, fun does1/4 }.
does1(SP,RP,IP,WP={Pos,Wft}) ->
    %% io:format("WP = ~w\n", [WP]),
    {XPos,Xt} = element(Pos, Wft),
    next([{Pos+1,Wft}|SP],[IP|RP],{XPos,Xt()},WP).

semis() ->    
    { 0, "(semis)", fun semis/4 }.
semis(SP,[IP|RP],_IP,WP) ->
    %% special treat when reach ';' while doinging create and 
    %% does> is not found
    case csp() of
	[?CF_CREATE|Csp] ->
	    Def = here(),
	    Name = element(2, Def),
	    define(Name, fun() -> Def end),
	    here({}),  %% clear defintion area
	    csp(Csp),  %% pop control stack
	    %% set_state(0),
	    next(SP,RP,IP,WP);
	_ ->
	    next(SP,RP,IP,WP)
    end.

branch() ->
    {0, <<"branch">>, fun branch/4 }.
branch(SP,RP,{IP,Code},WP) ->
    next(SP,RP,{IP+element(IP,Code),Code},WP).

zbranch() ->
    {0, <<"0branch">>, fun zbranch/4 }.
zbranch([0|SP],RP,{IP,Code},WP) ->
    Offset = element(IP,Code),
    next(SP,RP,{IP+Offset,Code},WP);
zbranch([_|SP],RP,{IP,Code},WP) ->
    next(SP,RP,{IP+1,Code},WP).

%% print stack value
'_ffe_dot'() ->
    {0, <<".">>, fun dot/4 }.
dot([Value|SP],RP,IP,WP) ->
    if is_integer(Value) ->
	    erlang:display_string(integer_to_list(Value, base()));
       is_float(Value) ->
	    erlang:display_string(io_lib_format:fwrite_g(Value));
       is_binary(Value) ->
	    erlang:display_string(binary_to_list(Value));
       is_pid(Value) ->
	    erlang:display_string(pid_to_list(Value));
       true -> %% hmm recursivly display integers in base()!
	    erlang:display_string(lists:flatten(io_lib:format("~p",[Value])))
    end,
    next(SP,RP,IP,WP).

%% words manuplating stack only

'_ffe_rote'() ->
    {0, <<"rot">>, fun rote/4}.
rote(SP,RP,IP,WP) ->
    ?rote(SP,RP,IP,WP,next).

'_ffe_rev_rote'() ->
    {0, <<"-rot">>, fun rev_rote/4}.
rev_rote(SP,RP,IP,WP) ->
    ?rev_rote(SP,RP,IP,WP,next).

'_ffe_plus'() ->
    { 0, <<"+">>, fun plus/4 }.
plus(SP,RP,IP,WP) ->
    ?plus(SP,RP,IP,WP, next).

'_ffe_one_plus'() ->
    { 0, <<"1+">>, fun one_plus/4 }.
one_plus(SP,RP,IP,WP) ->
    ?one_plus(SP,RP,IP,WP,next).

'_ffe_minus'() ->
    { 0, <<"-">>, fun minus/4 }.
minus(SP,RP,IP,WP) ->
    ?minus(SP,RP,IP,WP,next).

'_ffe_one_minus'() ->
    { 0, <<"1-">>, fun one_minus/4 }.
one_minus(SP,RP,IP,WP) ->
    ?one_minus(SP,RP,IP,WP,next).

'_ffe_star'() ->
    { 0, <<"*">>, fun star/4 }.
star(SP,RP,IP,WP) ->
    ?star(SP,RP,IP,WP,next).

'_ffe_slash'() ->
    { 0, <<"/">>, fun slash/4 }.
slash(SP,RP,IP,WP) ->
    ?slash(SP,RP,IP,WP,next).

'_ffe_mod'() ->
    { 0, <<"mod">>, fun mod/4 }.
mod(SP,RP,IP,WP) ->
    ?mod(SP,RP,IP,WP,next).

'_ffe_slash_mod'() ->
    { 0, <<"/mod">>, fun slash_mod/4 }.
slash_mod(SP,RP,IP,WP) ->
    ?slash_mod(SP,RP,IP,WP,next).

'_ffe_negate'() ->
    { 0, <<"negate">>, fun negate/4 }.
negate(SP,RP,IP,WP) ->
    ?negate(SP,RP,IP,WP,next).

'_ffe_over'() ->
    { 0, <<"over">>, fun over/4 }.
over(SP,RP,IP,WP) ->
    ?over(SP,RP,IP,WP,next).

'_ffe_drop'() ->
    { 0, <<"drop">>, fun drop/4 }.
drop(SP,RP,IP,WP) ->
    ?drop(SP,RP,IP,WP,next).

'_ffe_swap'() ->
    { 0, <<"swap">>, fun swap/4 }.
swap(SP,RP,IP,WP) ->
    ?swap(SP,RP,IP,WP,next).

'_ffe_dupe'() ->
    { 0, <<"dup">>, fun dupe/4 }.
dupe(SP,RP,IP,WP) ->
    ?dupe(SP,RP,IP,WP,next).

'_ffe_abs'() ->
    { 0, <<"abs">>, fun abs/4 }.
abs(SP,RP,IP,WP) ->
    ?abs(SP,RP,IP,WP,next).

'_ffe_lshift'() ->
    { 0, <<"lshift">>, fun lshift/4 }.
lshift(SP,RP,IP,WP) ->
    ?lshift(SP,RP,IP,WP,next).

'_ffe_rshift'() ->
    { 0, <<"rshift">>, fun rshift/4 }.
rshift(SP,RP,IP,WP) ->
    ?rshift(SP,RP,IP,WP,next).

'_ffe_arshift'() ->
    { 0, <<"arshift">>, fun arshift/4 }.
arshift(SP,RP,IP,WP) ->
    ?arshift(SP,RP,IP,WP,next).

'_ffe_and'() ->
    { 0, <<"and">>, fun 'and'/4 }.
'and'(SP,RP,IP,WP) ->
    ?'and'(SP,RP,IP,WP,next).

'_ffe_or'() ->
    { 0, <<"or">>, fun 'or'/4 }.
'or'(SP,RP,IP,WP) ->
    ?'or'(SP,RP,IP,WP,next).

'_ffe_invert'() ->
    { 0, <<"invert">>, fun invert/4 }.
invert(SP,RP,IP,WP) ->
    ?invert(SP,RP,IP,WP,next).

'_ffe_xor'() ->
    { 0, <<"xor">>, fun 'xor'/4 }.
'xor'(SP,RP,IP,WP) ->
    ?'xor'(SP,RP,IP,WP,next).

'_ffe_spat'() ->
    { 0, <<"sp@">>, fun spat/4 }.
spat(SP,RP,IP,WP) ->
    ?spat(SP,RP,IP,WP,next).

'_ffe_rpat'() ->
    { 0, <<"rp@">>, fun rpat/4 }.
rpat(SP,RP,IP,WP) ->
    ?rpat(SP,RP,IP,WP,next).

'_ffe_spstore'() ->
    { 0, <<"sp!">>, fun spstore/4 }.
spstore(SP,RP,IP,WP) ->
    ?spstore(SP,RP,IP,WP,next).

'_ffe_rpstore'() ->
    { 0, <<"rp!">>, fun rpstore/4 }.
rpstore(SP,_RP,IP,WP) ->
    ?rpstore(SP,_RP,IP,WP,next).

'_ffe_min'() ->
    { 0, <<"min">>, fun min/4 }.
min(SP,RP,IP,WP) ->
    ?min(SP,RP,IP,WP,next).

'_ffe_max'() ->
    { 0, <<"max">>, fun max/4 }.
max(SP,RP,IP,WP) ->
    ?max(SP,RP,IP,WP,next).

'_ffe_zero_equals'() ->
    { 0, <<"0=">>, fun zero_equals/4 }.
zero_equals(SP,RP,IP,WP) ->
    ?zero_equals(SP,RP,IP,WP,next).

'_ffe_zero_less'() ->
    { 0, <<"0<">>, fun zero_less/4 }.
zero_less(SP,RP,IP,WP) ->
    ?zero_less(SP,RP,IP,WP,next).

'_ffe_zero'() ->
    { 0, <<"0">>, fun docon/4, 0 }.

'_ffe_one'() ->
    { 0, <<"0">>, fun docon/4, 1 }.

'_ffe_minus_one'() ->
    { 0, <<"0">>, fun docon/4, -1 }.

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
    enter_compile(),
    next(SP,RP,IP,WP).

left_bracket() ->
    { ?IMMEDIATE, <<"[">>, fun left_bracket/4 }. 
left_bracket(SP,RP,IP,WP) ->
    leave_compile(),
    next(SP,RP,IP,WP).

string() ->
    { ?IMMEDIATE, <<"\"">>, fun string/4 }.
string(SP,RP,IP,WP) ->
    String = word($"),
    case is_compiling() of
	true ->
	    comma_(fun ffe:lit/0),
	    comma_(String),
	    next(SP,RP,IP,WP);
	false ->
	    next([String|SP],RP,IP,WP)
    end.

is_compiling() ->    
    (get_state() band ?COMPILE) =/= 0.

enter_compile() ->
    set_state(get_state() bor ?COMPILE).

leave_compile() ->
    set_state(get_state() band (bnot ?COMPILE)).

%% utils - fixme define in forth
compile_only() ->
    case is_compiling() of
	false -> throw({-14,compile_only});
	true -> ok
    end.

interpreting() ->
    case get_state() of
	0 -> ok;
	_ -> throw({-29, compiler_nesting})
    end.
