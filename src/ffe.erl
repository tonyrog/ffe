-module(ffe).

-export([start/0]).
-export([run/0]).
-export([define/2]).
-compile(export_all).

-define(INCLUDE_STRAP, true).

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
	 forth = [
		  internal_words(),
		  core_words(),
		  core_ext_words(),
		  common_words(),
		  tools_words(),
		  strap_words(),
		  floating:words(),
		  floating_ext:words()
		 ],
	 current = #{},    %% user defined words
	 var = #{},        %% variables Ref => Value
	 base = 10,
	 dpl = 0,
	 csp = [],
	 span = 0,
	 hld = 0,
	 latest = <<>>,
	 testu = 0
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

-define(UNTHREAD_NONE, 0).
-define(UNTHREAD_ALL, 1000000).

%%
%% Forth word layout:
%% { Flags :: smudge | immediate
%%   Name  :: binary()
%%   CFA   :: function/4
%%   PF1   :: function/0 | term
%%   PF2   :: function/0 | term
%%   ...
%%   PFn   :: function/0 | term
%% }
%%
-define(FFA, 1).
-define(NFA, 2).
-define(CFA, 3).
-define(PFA, 4).
-define(ff(W), element(?FFA,(W))).        %% flags field
-define(nf(W), element(?NFA,(W))).        %% name field
-define(cf(W), element(?CFA,(W))).        %% code field :: fun/4
-define(pf(I,W), element((?PFA-1)+(I),(W))). %% 1..n parameter field :: fun/0|term() 

-define(set_ff(W, Flags), setelement(1, (W), (Flags))).
-define(set_nf(W, Name), setelement(2, (W), (Name))).
-define(set_cf(W, Code), setelement(3, (W), (Code))).
-define(set_pf(I,W,Param), setelement(3+(I), (W), (Param))).

-include("ffe.hrl").

start() ->
    init(),
    main([]).

run() -> start().

compile(File) ->
    compile(File,[include,user]).

compile(File,Opt) ->
    init(),
    {ok,Fd} = file:open(File, []),
    set_user(#user.altin, Fd),
    try main([]) of 
	[] -> 
	    save(File,Opt);
	Stack ->
	    io:format("warning: stack element after compilation: ~p\n",
		      [Stack]),
	    save(File,Opt)
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
save(File,Opt) ->
    Module = filename:basename(File, ".fs"),
    MODULE = string:to_upper(Module),
    BootStrap = lists:member(bootstrap, Opt),
    Unthread = case lists:member(unthread, Opt) of
		   false -> ?UNTHREAD_NONE;
		   true -> ?UNTHREAD_ALL
	       end,
    Mode = case lists:member(include, Opt) of
	       true -> 
		   include;
	       false -> 
		   case lists:member(erlang, Opt) of
		       true -> erlang;
		       false -> include
		   end
	   end,
    User = lists:member(user, Opt),
    case Mode of
	include ->
	    if User -> Fd = user;
	       true -> {ok,Fd} = file:open(Module ++ ".i", [write])
	    end,
	    io:format(Fd, "-ifndef(__~s__).\n", [MODULE]),
	    io:format(Fd, "-define(__~s__, true).\n", [MODULE]);
       erlang ->
	    if User -> Fd = user;
	       true -> {ok,Fd}  = file:open(Module ++ ".erl", [write])
	    end,
	    io:format(Fd, "%% -*- erlang -*-\n", []),
	    io:format(Fd, "-module(~s).\n", [Module]),
	    io:format(Fd, "-compile(export_all).\n", [])
    end,
    %% module dictionary
    WordsName = if BootStrap; Mode =:= include ->
			Module++"_words";
		   true ->
			"words"
		end,
    if BootStrap ->
	    save_dict(Fd, ffe, Unthread, current()),
	    save_dict_words(Fd, ffe, WordsName, current());
       true ->
	    save_dict(Fd, Module, Unthread, current()),
	    save_dict_words(Fd, list_to_atom(Module), WordsName, current())
    end,
    if Mode =:= include ->
	    io:format(Fd, "-endif.\n", []);
	true ->
	    ok
    end,
    if not is_atom(Fd) ->
	    file:close(Fd);
       true  ->
	    ok
    end.

%% save all words in a dictionary, Module is the name of the
%% current module where the colon defined words are stored (local calls)
save_dict(Fd, Module, Unthread, Dict) ->
    %% generate word defintions, 
    maps:fold(
      fun(Name, Xt, Acc) ->
	      [save_def(Fd, Module, Name, Unthread, Xt) | Acc]
      end, [], Dict).

save_def(Fd, Module, Name, Unthread, Xt) ->
    W = Xt(),
    io:format(Fd, "~p() ->\n  {~w, ~p", 
	      [binary_to_atom(Name),?ff(W),?nf(W)]),
    save_words(Fd, Module, 3, Unthread, W),
    io:format(Fd, "}.\n", []),
    {Module,Name}.

save_words(Fd, Current, I, Unthread, Word) when I =< tuple_size(Word) ->
    save_word(Fd, Current, Unthread, element(I,Word)),
    save_words(Fd, Current, I+1, Unthread, Word);
save_words(_Fd, _Current, _I, _Unthread, _Word) ->
    ok.

save_word(Fd, Current, Unthread, W) when is_function(W) ->
    {arity,Arity} = erlang:fun_info(W, arity),
    {name,Name} = erlang:fun_info(W, name),
    {module,Module} = erlang:fun_info(W, module),
    if Arity =:= 0 ->
	    Def = W(),
	    DoCol = fun ffe:docol/4,
	    Semis = fun ffe:semis/4,
	    case ?cf(Def) of
		DoCol ->
		    if Unthread > 0 ->
			    save_words(Fd, Current, 4, Unthread-1, Def);
		       true ->
			    io:format(Fd, ",\n  fun ~p:~p/0", 
				      [list_to_atom(Current),
				       binary_to_atom(?nf(Def))])
		    end;
		Semis ->
		    if Unthread =:= ?UNTHREAD_ALL;
		       Unthread =:= 0 ->
			    io:format(Fd, ",\n  fun ffe:semis/0", []);
		       Unthread > 0 ->
			    ok
		    end;
		_ ->
		    io:format(Fd, ",\n  fun ~p:~p/0", 
			      [Module, Name])
	    end;
       true -> 
	    io:format(Fd, ",\n  fun ~p:~p/~w",
		      [Module, Name, Arity])
    end;
save_word(Fd, _Current, _Unthread, W) -> %% literals etc
    io:format(Fd, ",\n  ~p", [W]).

%% "save" the dictionary
save_dict_words(Fd, Module, WordsName, Dict) when is_atom(Module) ->
    io:format(Fd, "~s() ->\n  #{\n", [WordsName]),
    maps:fold(
      fun(Name, _Xt, _Acc) ->
	      io:format(Fd, "      <<\"~s\">> => fun ~p:~p/0,\n",
			[Name, Module, binary_to_atom(Name)])
      end, [], Dict),
    io:format(Fd, "      <<>> => false\n", []),  %% dummy clause
    io:format(Fd, "  }.\n", []).


show_def(Fd, Name, Unthread, Xt) ->
    W = Xt(),
    emit_string(Fd, ": "),
    emit_string(Fd, Name),
    show_words_(Fd, 4, Unthread, W),
    emit_string(Fd, " ;"),
    Name.

show_words_(Fd, I, Unthread, Word) when I =< tuple_size(Word) ->
    show_word_(Fd, Unthread, element(I, Word)),
    show_words_(Fd, I+1, Unthread, Word);
show_words_(_Fd, _I, _Unthread, _Word) ->
    ok.

show_word_(Fd, Unthread, W) when is_function(W) ->
    {arity,Arity} = erlang:fun_info(W, arity),
    {name,FName} = erlang:fun_info(W, name),
    {module,Module} = erlang:fun_info(W, module),
    if Arity =:= 0 ->
	    Def = W(),
	    DoCol = fun ffe:docol/4,
	    Semis = fun ffe:semis/4,
	    ModStr = if Module =:= ffe -> "";
			true -> atom_to_list(Module)++":"
		     end,
	    case ?cf(Def) of
		DoCol ->
		    Def = W(),
		    if Unthread > 0 ->
			    show_words_(Fd, 4, Unthread-1, Def);
		       true ->
			    Name = ?nf(Def),
			    emit_char(Fd, $\s),
			    emit_string(Fd, ModStr),
			    emit_string(Fd, Name)
		    end;
		Semis ->
		    ok;
		_ ->
		    Name = ?nf(Def),
		    emit_char(Fd, $\s),
		    emit_string(Fd, ModStr),
		    emit_string(Fd, Name)		    
	    end;
       true ->
	    emit_string(Fd, atom_to_list(Module)),
	    emit_char(Fd, $:),
	    emit_string(Fd, atom_to_list(FName)),
	    emit_char(Fd, $/),
	    emit_string(Fd, integer_to_list(Arity))
    end;
show_word_(Fd, _Unthread, W) ->
    emit_char(Fd, $\s),
    emit_value(Fd, W).

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

get_base() -> get_user(#user.base).
set_base(Base) when is_integer(Base), Base > 1, Base =< 36 ->
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

get_variables() -> get_user(#user.var).
set_variables(VarMap) -> set_user(#user.var, VarMap).

get_value(Var) ->
    maps:get(Var, get_variables(), 0).

set_value(Var, Value) ->
    set_variables(maps:put(Var, Value, get_variables())).

%% Add Name to current vocabulary
define(Name,W) ->
    %% io:format("define ~s = ~p = ~p\n", [Name, W, W()]),
    current(maps:put(Name,W,current())).

%% FIRST VERSION in Erlang - to validate the idea
%% REWRITE in forth!
%%
quit0() ->
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
    try to_integer(Name, get_base()) of
	Int when Compile ->
	    comma_(fun ffe:lit/0),
	    comma_(Int),
	    main(SP);
	Int ->
	    main([Int|SP])
    catch
	error:_ ->
	    %% FIXME: check for floating support!
	    try binary_to_float(Name) of
		Flt when Compile ->
		    comma_(fun ffe:lit/0),
		    comma_(Flt),
		    main(SP);
		Flt ->
		    main([Flt|SP])
	    catch
		?EXCEPTION(error,_Error,_Trace) ->
		    io:format("~s ?\n", [Name]),
		    quit0()
	    end
    end.

%% $abc ignore base and use hex number
to_integer(<<$$,Cs/binary>>, _Base) ->
    binary_to_integer(Cs, 16);
to_integer(Cs, Base) ->
    binary_to_integer(Cs, Base).

%% stub for return after exec
exec_ret() ->
    {0,<<"(ret)">>,fun ?MODULE:exec_ret/4,fun ?MODULE:ret/0}.
exec_ret(_SP,_RP,_IP,_WP) ->
    erlang:display("INTERNAL ERROR\n"),
    throw({?QUIT, quit}).

exec(W, SP) ->
    CFA = ?cf(W),
    R = exec_ret(),
    try CFA(SP,[],?WPTR(4,R),?WPTR(4,W)) of
	SP1 -> main(SP1)
    catch
	throw:{?BYE=_Code,_Reason} ->
	    halt(0); %% ??
	throw:{?QUIT=_Code,_Reason} ->
	    quit0();
	throw:{?UNDEF=Code,Reason} ->
	    io:format("~w : ~p\n", [Code, Reason]),
	    quit0();
	throw:{Code,Reason} ->
	    io:format("~w : ~w\n", [Code, Reason]),
	    quit0();
	?EXCEPTION(error,{case_clause,SP0},_StackTrace) when is_list(SP0) ->
	    CallStack = ?GET_STACK(_StackTrace),
	    Calls = string:join(callstack(CallStack), " "),
	    %% fixme: print forth word where error occured 
	    io:format("Stack empty\n", []),
	    io:format("  call: ~p\n", [Calls]),
	    io:format("  call stack: ~p\n", [CallStack]),
	    quit0();
	?EXCEPTION(error,function_clause,_StackTrace) ->
	    CallStack = ?GET_STACK(_StackTrace),
	    Calls = string:join(callstack(CallStack), " "),
	    %% fixme: print forth word where error occured 
	    io:format("Stack empty\n", []),
	    io:format("  call: ~p\n", [Calls]),
	    io:format("  call stack: ~p\n", [CallStack]),
	    quit0();

	?EXCEPTION(error,Reason,_StackTrace) ->
	    CallStack = ?GET_STACK(_StackTrace),
	    io:format("Internal error: ~p\n", [Reason]),
	    io:format("  call stack: ~p\n", [CallStack]),	    
	    quit0()
    end.

callstack([{ffe,Word,_,_}|Stack]) ->
    [atom_to_list(Word) | callstack(Stack)];
callstack([_ | _Stack]) ->
    ["..."];
callstack([]) ->
    [].


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
		    %%F = binary_to_atom(<< <<"_ffe_">>/binary,Func/binary>>, latin1),
		    F = binary_to_atom(Func, latin1),
		    %% should we check remote calls for immediate ?
		    %% what if module does not exist yet? tricky!
		    %% fun () -> apply(M, F, []) end
		    {false, erlang:make_fun(M,F,0)};
		_ ->
		    false
	    end;
	{ok,Xt} ->
	    W = Xt(),
	    if ?ff(W) band ?IMMEDIATE  =:= ?IMMEDIATE ->
		    {true,Xt};
	       true ->
		    {false,Xt}
	    end
    end.

lookup_word_(Name) ->
    lookup_word_(Name, [current() | forth()]).

lookup_word_(Name, [Dict|Ds]) ->
    case maps:find(Name, Dict) of
	error ->
	    lookup_word_(Name, Ds);
	Found -> Found
    end;
lookup_word_(_Name, []) ->
    error.

%% TIB  = line buffer
%% SPAN = offset in line buffer
word(Ch) ->
    parse_(Ch,true).
parse(Ch) ->
    parse_(Ch,false).

parse_(Ch,DoDrop) ->
    In = in(),  %% >in @
    case tib() of
	Tib when In >= byte_size(Tib) ->
	    case refill() of
		eof -> eof;
		_Count -> parse_(Ch,DoDrop)
	    end;
	<<_:In/binary,Data/binary>> ->
	    case enclose(Ch,Data,DoDrop) of
		[_N1,0,0] when not DoDrop->
		    in(In+1),
		    <<>>;
		[_N1,N2,0] ->
		    in(In+N2+1),
		    parse_(Ch,DoDrop);
		[N1,N2,Len] ->
		    <<_:N1/binary,Word:Len/binary,_/binary>> = Data,
		    in(In+N2+1),
		    Word
	    end
    end.

%% TIB  = line buffer
%% SPAN = offset in line buffer
char() ->
    In = in(),  %% >in @
    case tib() of
	Tib when In >= byte_size(Tib) ->
	    case refill() of
		eof -> eof;
		_Count -> char()
	    end;
	<<_:In/binary,Char,_/binary>> ->
	    in(In+1),
	    Char
    end.

%% read a line and put it in TIB 
refill() ->
    ALTIN = altin(),
    ALTOUT = altout(),
    case is_compiling() of
	false when ALTIN =:= standard_io, ALTOUT =:= standard_io ->
	    emit_string("ok ");
	_ ->
	    ok
    end,
    case io:get_line(ALTIN, '') of
	eof ->
	    source(<<>>,0),
	    eof;
	Data ->
	    out(0), %%? ok?
	    Data1 = erlang:iolist_to_binary(Data),
	    Sz1 = byte_size(Data1)-1,
	    %% strip newline
	    case Data1 of
		<<Data2:Sz1/binary,$\n>> -> source(Data2,0);
		Data2 -> source(Data2,0)
	    end
    end.

emit_char(Char) ->
    emit_char(altout(), Char).

emit_char(Fd, Char) ->
    if Char =:= $\n ->
	    io:put_chars(Fd,[$\n]),
	    out(0);
       true ->
	    io:put_chars(Fd,[Char]),
	    out(out()+1)
    end.

emit_chars(Binary) when is_binary(Binary) ->
    emit_chars_(altout(), byte_size(Binary), binary_to_list(Binary));
emit_chars(Chars) ->
    emit_chars_(altout(), length(Chars), Chars).

emit_chars(Fd, Len, Binary) when is_binary(Binary) ->
    emit_chars_(Fd, Len, binary_to_list(Binary));
emit_chars(Fd, Len, Chars) when is_list(Chars) ->
    emit_chars_(Fd, Len, Chars).

emit_chars_(Fd, Len, Chars) ->
    io:put_chars(Fd, Chars),
    out(out()+Len).

emit_string(Binary) when is_binary(Binary) ->
    emit_string_(altout(), binary_to_list(Binary));
emit_string(Chars) when is_list(Chars) ->
    emit_string_(altout(), Chars).

emit_string(Fd, Binary) when is_binary(Binary) ->
    emit_string_(Fd, binary_to_list(Binary));
emit_string(Fd,Chars) when is_list(Chars) ->
    emit_string_(Fd, Chars).

emit_string_(_Fd, []) ->
    ok;
emit_string_(Fd, Cs) ->
    case collect_line(Cs, 0, []) of
	{true,Len,Chars,Cs1} ->
	    emit_chars_(Fd,Len,Chars),
	    emit_char(Fd,$\n),
	    emit_string_(Fd, Cs1);
	{false,Len,Chars,Cs1} ->
	    emit_chars_(Fd,Len,Chars),
	    emit_string_(Fd, Cs1)
    end.

collect_line([$\n|Cs], N, Acc) ->
    {true, N, lists:reverse(Acc), Cs};
collect_line([C|Cs], N, Acc) ->
    collect_line(Cs, N+1, [C|Acc]);
collect_line([], N, Acc) ->
    {false, N, lists:reverse(Acc), []}.

%%  ( ch c-addr len -- c-addr n1 n2 n3 )
%%  n1 = offset to first none ch char (word start)
%%  n2 = offset to last char in word (word stop)
%%  n3 = length of enclosed data
enclose(Ch,Data,DoDrop) ->
    Len = byte_size(Data),
    N1 = if DoDrop -> drop(Ch, Data, 0); true -> 0 end,
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
    
%% "primitive" forth words, we may compile some of them soon
%%  this "dictionary" should/must? be constant
internal_words() ->
    #{
      ?WORD("(docol)",    docol),
      ?WORD("(dousr)",    dousr),
      ?WORD("(docon)",    docon),
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
      ?WORD("noop", noop),
      ?WORD("base", base),
      ?WORD("testu", testu),  %% TESTING!!! user area
      
      ?WORD("sp@",        spat),
      ?WORD("rp@",        rpat),
      ?WORD("sp!",        spstore),
      ?WORD("rp!",        rpstore),
      ?WORD("i",          i),
      ?WORD("j",          j),
      ?WORD("leave",      leave),

      ?WORD("-rot",       rrot),

      ?WORD("0",          zero),
      ?WORD("1",          one),
      ?WORD("-1",         minus_one),
      
      %% system 
      ?WORD("bye",        bye),
      
      ?WORD("create",     create),
      ?WORD("does>",      does),
      
      %% meta words - pre compile
      ?WORD("constant",   compile_constant),
      ?WORD("variable",   compile_variable),
      ?WORD("value",      compile_value),
      ?WORD("user",       compile_user),
      ?WORD("to",         to),
      ?WORD("do",         compile_do),
      ?WORD("?do",        compile_qdo),
      ?WORD("loop",       compile_loop),
      ?WORD("if",         compile_if),
      ?WORD("then",       compile_then),
      ?WORD("else",       compile_else),
      
      ?WORD("\\",         backslash),
      ?WORD("(",          paren),
      ?WORD("(lit)",      lit),
      ?WORD("literal",    literal),
      ?WORD("\"",         string),

      ?WORD("char",       care),
      %% Utils - will be strapped later

      ?WORD("remove",     remove_word),
      ?WORD("show",       show_word),
      ?WORD("unthread",   unthread_word)
     }.

%%
%% General word layout
%% {Flags, <<"Name">>, fun cfa/4, fun pf1/0, ... fun pfn/0}
%%
next(SP,RP,?WPTR(IP,Code),?WPTR(_WP,_W)) ->
    PF = element(IP, Code),  %% code parameter 
    W = PF(),                %% tuple word
    CFA = ?cf(W),            %% read CFA
    PFA1 = ?WPTR(4,W),       %% point to first "parameter"
    CFA(SP,RP,?WPTR(IP+1,Code),PFA1).

add_addr(?WPTR(Y,W), X) when is_integer(X) -> ?WPTR(Y+X,W);
add_addr(Y, X) when is_integer(Y) -> Y+X.

%% fetch value at address
fetch_at(Addr) ->
    case Addr of
	{var,Var} ->
	    get_value(Var);
	{user,Field} ->
	    case get_user(Field) of
		undefined -> 0;
		V -> V
	    end;
	{sys,_} ->
	    case ets:lookup(forth,Addr) of
		[] -> 0;
		[{_,V}] -> V
	    end;
	?WPTR(I,W) ->
	    element(I,W)
    end.

store_at(Addr,X) ->
    case Addr of
	{var,Var} ->
	    set_value(Var, X);
	{user,Field} -> 
	    set_user(Field, X);
	{sys,_} ->
	    ets:insert(forth,{Addr,X})
    end.

smudge() ->
    { 0, <<"smudge">>, fun ffe:smudge/4 }.
smudge(SP, RP, IP, WP) ->
    Def = here(),
    Def1 = ?set_ff(Def, ?ff(Def) bxor ?SMUDGE),
    here(Def1),
    next(SP, RP, IP, WP).


%% CONSTANT
?XT("constant", compile_constant).
compile_constant([Value|SP],RP,IP,WP) ->
    Name = word($\s),
    Def = {0, Name, fun ?MODULE:docon/4, Value }, 
    define(Name, fun() -> Def end),
    next(SP,RP,IP,WP).

%% VARIABLE
?XT("variable", compile_variable).
compile_variable(SP,RP,IP,WP) ->
    Name = word($\s),
    Var = make_variable_ref(),
    Def = {0, Name, fun ?MODULE:dovar/4, Var },
    define(Name, fun() -> Def end),
    next(SP,RP,IP,WP).

%% VALUE
?XT("value", compile_value).
compile_value([Value|SP],RP,IP,WP) ->
    Name = word($\s),
    Var = make_variable_ref(),
    set_value(Var, Value),
    Def = {0, Name, fun ?MODULE:doval/4, Var },
    define(Name, fun() -> Def end),
    next(SP,RP,IP,WP).

make_variable_ref() ->
    {var, erlang:unique_integer([])}.

compile_user() ->
    { 0, <<"user">>, fun ?MODULE:compile_user/4 }.
compile_user([Value|SP],RP,IP,WP) ->
    Name = word($\s),
    Def = {0, Name, fun ?MODULE:dousr/4, Value },
    define(Name, fun() -> Def end),
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
?IXT("do", compile_do).
compile_do(SP,RP,IP,WP) ->
    compile_only(),
    cf_push(here_()), cf_push(?CF_DO_SYS),
    comma_(fun ffe:pdo/0),
    next(SP,RP,IP,WP).

%% ?DO immediate word
?IXT("?do", compile_qdo).
compile_qdo(SP,RP,IP,WP) ->
    compile_only(),
    cf_push(here_()), cf_push(?CF_QDO_SYS),
    comma_(fun ffe:pqdo/0),
    comma_(0),  %% patch this place
    next(SP,RP,IP,WP).

%% LOOP immediate word
?IXT("loop", compile_loop).
compile_loop(SP,RP,IP,WP) ->
    compile_only(),
    case cf_pop() of
	?CF_DO_SYS ->
	    comma_(fun ?MODULE:ploop/0),
	    Pos = cf_pop(),
	    back_(Pos+1);
	?CF_QDO_SYS ->
	    comma_(fun ?MODULE:ploop/0),
	    Pos = cf_pop(),
	    back_(Pos+2),
	    forward_patch_(Pos+2);
	_ -> 
	    throw({-22, "loop missing DO/?DO"})
    end,
    next(SP,RP,IP,WP).

?IXT("if", compile_if).
compile_if(SP,RP,IP,WP) ->
    compile_only(),
    comma_(fun ?MODULE:zbranch/0),    %% postpone 0branch
    cf_push(here_()), cf_push(?CF_ORIG),
    comma_(0),  %% patch this place
    next(SP,RP,IP,WP).

?IXT("then", compile_then).
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

?IXT("else", compile_else).
compile_else(SP,RP,IP,WP) ->
    compile_only(),
    case cf_pop() of
	?CF_ORIG ->
	    Pos = cf_pop(),
	    comma_(fun ?MODULE:branch/0),    %% postpone 0branch
	    cf_push(here_()), cf_push(?CF_ORIG),
	    comma_(0),  %% patch this place
	    forward_patch__(Pos),
	    next(SP,RP,IP,WP);
	_ ->
	    throw({-22, "ELSE missing IF"})
    end.

?XT("create", create).
create(SP,RP,IP,WP) ->
    Name = word($\s),
    %% io:format("create word ~s\n", [Name]),
    Does0 = {0,Name,fun ?MODULE:does0/4, 0},
    here(Does0),
    cf_push(?CF_CREATE),
    next(SP, RP, IP, WP).

?XT("does>", does).
does(SP,RP,IP,WP) ->
    case csp() of
	[?CF_CREATE|Csp] -> %% only in create?
	    Def0 = here(),
	    Name = ?nf(Def0),
	    Def1 = ?set_cf(Def0, fun ?MODULE:does1/4),
	    ?WPTR(DoesPos,DoesCode) = IP,
	    DoesName = ?nf(DoesCode),
	    {_, DoesXt} = find_word_(DoesName),
	    Def2 = ?set_pf(1,Def1,?WPTR(DoesPos,DoesXt)),
	    define(Name, fun() -> Def2 end),
	    here({}),  %% clear defintion area
	    csp(Csp),  %% pop control stack
	    [IP1|RP1] = RP,
	    next(SP,RP1,IP1,WP)
    end.
    
?XT("(do)", pdo).
pdo(_SP0=[I,N|SP],RP,IP,WP) ->
    next(SP,[I,N|RP],IP,WP).

%% ?DO check condition before the loop
?XT("(?do)", pqdo).
pqdo([I,N|SP],RP,?WPTR(IP,Code),WP) when I < N ->
    next(SP,[I,N|RP],?WPTR(IP+1,Code),WP);
pqdo([_I,_N|SP],RP,?WPTR(IP,Code),WP) ->
    next(SP,RP,?WPTR(IP+element(IP,Code),Code),WP).

?XT("(loop)", ploop).
ploop(SP,[I|RP=[N|RP1]],?WPTR(IP,Code),WP) ->
    if I < N-2 ->
	    next(SP,[I+1|RP],?WPTR(IP+element(IP,Code),Code),WP);
       true ->
	    next(SP,RP1,?WPTR(IP+1,Code),WP)
    end.

?XT("i", i).
i(SP,[Ix|_]=RP,IP,Code) ->
    next([Ix|SP],RP,IP,Code).

?XT("j", j).
j(SP,[_,_,Jx|_]=RP,IP,Code) ->
    next([Jx|SP],RP,IP,Code).

?XT("base", base).
base(SP,RP,IP,Code) ->
    next([{user,#user.base}|SP],RP,IP,Code).

?XT("textu", testu).
testu(SP,RP,IP,Code) ->
    next([{user,#user.testu}|SP],RP,IP,Code).

?XT("to", to).
to([Value|SP],RP,IP,Code) ->
    Name = word($\s),
    case find_word_(Name) of
	{_, Xt} ->
	    Var = element(4, Xt()),
	    set_value(Var, Value),
	    next(SP,RP,IP,Code);
	false ->
	    throw({?UNDEF, Name})
    end.

?XT("leave", leave).
leave(SP,[_,RP=[Limit|_]],IP,WP) ->
    next(SP,[Limit|RP],IP,WP).

?XT("bye", bye).
bye(_SP,_RP,_IP,_WP) ->
    throw({?BYE, exit}).

?XT("ref", ret).
ret(SP,_RP,_IP,_Code) ->
    SP.

?XT("noop", noop).
noop(SP,RP,I,Code) ->
    next(SP,RP,I,Code).

?XT("(lit)", lit).
lit(SP,RP,?WPTR(IP,Code),WP) ->
    next([element(IP,Code)|SP],RP,?WPTR(IP+1,Code),WP).

?IXT("literal", literal).
literal(SP,RP,IP,WP) ->
    compile_only(),
    [X|SP1] = SP,
    comma_(fun ffe:lit/0),
    comma_(X),
    next(SP1,RP,IP,WP).

execut() ->
    { 0, <<"execut">>, fun ffe:execut/4 }.
execut([Xt|SP],RP,IP,_WP) ->
    W = Xt(),            %% tuple word
    CFA = ?cf(W),        %% get code field
    CFA(SP,RP,IP,?WPTR(?PFA,W)).

%% WP points to current executing word, move that to point to the
%% new word PFA area
dodoes(SP,RP,IP,WP=?WPTR(Wi,W)) ->
    W1 = element(Wi-1,W()),
    WP1 = ?WPTR(?PFA,W1),
    docol([WP|SP],RP,IP,WP1).

docol() ->
    { 0, <<"(docol)">>, fun ffe:docol/4 }.
docol(SP,RP,IP,WP) ->
    next(SP,[IP|RP],WP,WP).

docon() ->
    { 0, <<"(docon)">>, fun ffe:docon/4 }.
docon(SP,RP,IP,WP0=?WPTR(W,WP)) ->
    %% io:format("do con WP0 = ~w\n", [WP0]),
    next([element(W,WP)|SP],RP,IP,WP0).

dousr() ->
    { 0, "(dousr)", fun ffe:dousr/4 }.
dousr(SP,RP,IP,WP0=?WPTR(W,WP)) ->
    next([{user,element(W,WP)}|SP],RP,IP,WP0).

dovar() ->
    { 0, <<"(dovar)">>, fun ffe:dovar/4 }.
dovar(SP,RP,IP,WP0=?WPTR(W,WP)) ->
    next([element(W,WP)|SP],RP,IP,WP0).

doval() ->
    { 0, <<"(doval)">>, fun ffe:doval/4 }.
doval(SP,RP,IP,WP0=?WPTR(W,WP)) ->
    next([get_value(element(W,WP))|SP],RP,IP,WP0).

%% default does code with no offset
does0(SP,RP,?WPTR(IP,Code),WP=?WPTR(Pos,Wf)) ->
    next([?WPTR(Pos+1,Wf)|SP],RP,?WPTR(IP+1,Code),WP).

%% does with offset to does> code
does1() ->
    { 0, <<"(does>)">>, fun ?MODULE:does1/4 }.
does1(SP,RP,IP,WP=?WPTR(Pos,Wft)) ->
    %% io:format("WP = ~w\n", [WP]),
    ?WPTR(XPos,Xt) = element(Pos, Wft),
    next([?WPTR(Pos+1,Wft)|SP],[IP|RP],?WPTR(XPos,Xt()),WP).

semis() ->    
    { 0, "(semis)", fun ?MODULE:semis/4 }.
semis(SP,[IP|RP],_IP,WP) ->
    %% special treat when reach ';' while doing create and 
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
    {0, <<"branch">>, fun ffe:branch/4 }.
branch(SP,RP,?WPTR(IP,Code),WP) ->
    next(SP,RP,?WPTR(IP+element(IP,Code),Code),WP).

zbranch() ->
    {0, <<"0branch">>, fun ffe:zbranch/4 }.
zbranch([0|SP],RP,?WPTR(IP,Code),WP) ->
    Offset = element(IP,Code),
    next(SP,RP,?WPTR(IP+Offset,Code),WP);
zbranch([_|SP],RP,{IP,Code},WP) ->
    next(SP,RP,?WPTR(IP+1,Code),WP).

cr() ->
    {0, <<"cr">>, fun ffe:cr/4 }.
cr(SP,RP,IP,WP) ->
    emit_char($\n),
    next(SP,RP,IP,WP).

count() ->
    {0, <<"count">>, fun ffe:count/4 }.
count(SP=[Addr|_],RP,IP,WP) ->
    if is_binary(Addr) ->
	    next([byte_size(Addr)|SP],RP,IP,WP);
       true ->
	    next([0|SP],RP,IP,WP)
    end.

?XT("type", type).
type([U,Addr|SP],RP,IP,WP) ->
    if is_binary(Addr) ->
	    emit_chars(altout(), U, Addr),
	    next(SP,RP,IP,WP);
       true ->
	    next(SP,RP,IP,WP)
    end.

?XT("space", space).
space(SP,RP,IP,WP) ->
    emit_char($\s),
    next(SP,RP,IP,WP).

?XT("spaces", spaces).
spaces([U|SP],RP,IP,WP) ->
    emit_chars(lists:duplicate(U,$\s)),
    next(SP,RP,IP,WP).

%% char
?XT("char", care).
care(SP,RP,IP,WP) ->
    Char = char(),
    next([Char|SP],RP,IP,WP).

%% words manuplating stack only

m_rot() ->
    {0, <<"-rot">>, fun ffe:m_rot/4}.
m_rot(SP,RP,IP,WP) ->
    ?m_rot(SP,RP,IP,WP,next).

spat() ->
    { 0, <<"sp@">>, fun ffe:spat/4 }.
spat(SP,RP,IP,WP) ->
    ?spat(SP,RP,IP,WP,next).

rpat() ->
    { 0, <<"rp@">>, fun ffe:rpat/4 }.
rpat(SP,RP,IP,WP) ->
    ?rpat(SP,RP,IP,WP,next).

spstore() ->
    { 0, <<"sp!">>, fun ffe:spstore/4 }.
spstore(SP,RP,IP,WP) ->
    ?spstore(SP,RP,IP,WP,next).

rpstore() ->
    { 0, <<"rp!">>, fun ffe:rpstore/4 }.
rpstore(SP,_RP,IP,WP) ->
    ?rpstore(SP,_RP,IP,WP,next).

zero() ->
    { 0, <<"0">>, fun ffe:docon/4, 0 }.

one() ->
    { 0, <<"1">>, fun ffe:docon/4, 1 }.

minus_one() ->
    { 0, <<"-1">>, fun ffe:docon/4, -1 }.

%% IMMEDIATE
backslash() ->
    { ?IMMEDIATE, <<"\\">>, fun ffe:backslash/4 }.
backslash(SP,RP,IP,WP) ->
    in(span()),  %% skip all characters in input buffer
    next(SP,RP,IP,WP).

paren() -> 
    { ?IMMEDIATE, <<"(">>, fun ffe:paren/4 }. 
paren(SP,RP,IP,WP) ->
    word($)),       %% skip until ')'
    next(SP,RP,IP,WP).

string() ->
    { ?IMMEDIATE, <<"\"">>, fun ffe:string/4 }.
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

emit_value(Value) ->
    emit_value(altout(), Value).
emit_value(Fd, Value) ->
    if is_integer(Value) ->
	    emit_string(Fd,integer_to_list(Value, get_base()));
       is_float(Value) ->
	    emit_string(Fd,io_lib_format:fwrite_g(Value));
       is_binary(Value) ->
	    emit_string(Fd,Value);
       is_pid(Value) ->
	    emit_string(Fd,pid_to_list(Value));
       true -> %% hmm recursivly display integers in base()!
	    emit_string(Fd,lists:flatten(io_lib:format("~p",[Value])))
    end.    


%% output a word as erlang code - fixme only allow colon defs!
?XT("show", show_word).
show_word(SP,RP,IP,WP) ->
    Name = word($\s),
    case find_word_(Name) of
	{_, Xt} ->
	    show_def(user, Name, ?UNTHREAD_NONE, Xt);
	false ->
	    throw({?UNDEF, Name})
    end,
    next(SP,RP,IP,WP).

%% unthread a word
?XT("unthread", unthread_word).
unthread_word(SP,RP,IP,WP) ->
    Name = word($\s),
    case find_word_(Name) of
	{_, Xt} ->
	    save_def(user, ffe, Name, ?UNTHREAD_ALL, Xt);
	false ->
	    throw({?UNDEF, Name})
    end,
    next(SP,RP,IP,WP).

%% Remove word from current dictionary
%% Remember that words that refer to this word will 
%% continue to work in interactive mode but will not compile!
?XT("remove", remove_word).
remove_word(SP,RP,IP,WP) ->
    Name = word($\s),
    Current = maps:remove(Name, current()),
    current(Current),
    next(SP,RP,IP,WP).

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

-include("core.i").
-include("core_ext.i").
-include("common.i").
%%-include("search.i").
-include("tools.i").
%%-include("tools_ext.i").

-ifdef(INCLUDE_STRAP).
-include("strap.i").
-else.
strap_words() ->
    #{}.
-endif.
