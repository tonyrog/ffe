%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    FFE
%%% @end
%%% Created : 30 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(ffe).

-export([start/0]).
-export([run/0]).
-export([define/2]).
-export([call/2]).
-compile(export_all).

-define(INCLUDE_STRAP, true).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

-include("ffe.hrl").

%% State flags
-define(COMPILE,  16#01).
-define(NONAME,   16#02).

%% dictionary word status
-define(SMUDGE,      16#40).
-define(IMMEDIATE,   16#80).
%% maybe a ICASE flags? 
%% lookup WoRd, fail lookup to_lower(word) if found and ?ICASE then ok!
-define(ICASE,       16#20).

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

-define(MAX_STACKTRACE_DEPTH, 10).

-type prim_buffer() :: reference().

-record(user,
	{
	 tib :: undefined | prim_buffer(),  %% text input buffer
	 pad :: undefined | prim_buffer(),  %% scratch input buffer
	 width = 0,   %% max width of word (fix me not used)
	 dp = {},     %% data pointer (fixme)
	 tty :: undefined | port(),  %% terminal input
	 altout =  ?STANDARD_OUTPUT,
	 in = 0,
	 out = 0,
	 state = 0,
	 forth = [
		  internal_words(),
		  core_words(),
		  core_ext_words(),
		  common_words(),
		  file_words(),
		  tools_words(),
		  tools_ext_words(),
		  facility_words(),
		  strap_words(),
		  floating:words(),
		  floating_ext:words()
		 ],
	 current = #{},      %% user defined words
	 var = #{},          %% variables Ref => Value
	 argv = {<<"ffe">>}, %% ffe command line arguments
	 base = 10,          %% current base
	 dpl = 0,
	 csp = [],           %% compile control stack
	 hsp = [],           %% hold area, stack!
	 exceptions = [],    %% exception stack
	 blk = 0,
	 source_id = ?TERMINAL_INPUT,      
	 span = 0,
	 hld = 0,
	 latest = <<>>       %% latest word defined
	}).

start() ->
    init(),
    main([]),
    halt(0).

run() -> start().

compile(File) ->
    compile(File,[include,user]).

compile(Filename,Opt) ->
    init(),
    [0,Fd] = file_open(Filename, [raw,read,binary]),
    set_source_id(Fd),
    try main([]) of 
	[] -> 
	    save(Filename,Opt);
	Stack ->
	    Msg = io_lib:format("warning: stack element after compilation: ~p",
				[Stack]),
	    ffe_tio:output(?STANDARD_OUTPUT, [Msg,?CRNL]),
	    save(Filename,Opt)
    catch
	throw:{Code,Reason} ->
	    Msg = io_lib:format("error: ~w reason: ~p",[Code,Reason]),
	    ffe_tio:output(?STANDARD_OUTPUT, [Msg,?CRNL]);
	?EXCEPTION(error,Error,_StackTrace) ->
	    Msg = io_lib:format("error: internal error ~p", [Error]),
	    ffe_tio:output(?STANDARD_OUTPUT, [Msg,?CRNL]),
	    dump_stacktrace(?GET_STACK(_StackTrace))
    after
	file:close(Fd)
    end.

%% Save words as erlang source code
%% use ffe_beam to save directly as beam code
%% go through dictionary and write them as word defintions.
save(Filename,Opt) ->
    Module = filename:basename(Filename, ".fs"),
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
	    if User ->
		    Fd = ?STANDARD_OUTPUT;
	       true ->
		    [0,Fd] = file_open(Module ++ ".i", [raw,write])
	    end,
	    emit_strings(Fd, ["-ifndef(__",MODULE,"__).\n"]),
	    emit_strings(Fd, ["-define(__",MODULE,"__, true).\n"]);
       erlang ->
	    if User ->
		    Fd = ?STANDARD_OUTPUT;
	       true ->
		    [0,Fd] = file_open(Module ++ ".erl", [raw,write])
	    end,
	    emit_string(Fd, "%% -*- erlang -*-\n"),
	    emit_strings(Fd, ["-module(",Module,").\n"]),
	    emit_string(Fd, "-compile(export_all).\n")
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
	    emit_string(Fd, "-endif.\n");
	true ->
	    ok
    end,
    if is_integer(Fd), Fd > 2 ->
	    case file_close(Fd) of
		0 -> ok;
		IOR -> {error, IOR}
	    end;
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
    emit_strings(Fd,[Name,"() ->\n  {",
		     integer_to_list(?ff(W)),
		     ", ",
		     "<<",?nf(W),">>"]),
    save_words(Fd, Module, 3, Unthread, W),
    emit_string(Fd, "}.\n"),
    {Module,Name}.

save_words(Fd, Current, I, Unthread, Word) when I =< tuple_size(Word) ->
    save_word(Fd, Current, Unthread, element(I,Word)),
    save_words(Fd, Current, I+1, Unthread, Word);
save_words(_Fd, _Current, _I, _Unthread, _Word) ->
    ok.

save_word(Fd, Current, Unthread, W) when is_function(W) ->
    {arity,Arity}   = erlang:fun_info(W, arity),
    {name,Name}     = erlang:fun_info(W, name),
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
			    emit_strings(fd,[",\n  fun ",
					     format_atom(Current),
					     ":", ?nf(Def), "/0"])
		    end;
		Semis ->
		    if Unthread =:= ?UNTHREAD_ALL;
		       Unthread =:= 0 ->
			    emit_string(Fd, ",\n  fun ffe:semis/0");
		       Unthread > 0 ->
			    ok
		    end;
		_ ->
		    emit_strings(Fd, [",\n  fun " |
				      format_mfa(Module,Name, 0)])
	    end;
       true -> 
	    emit_strings(Fd, [",\n  fun " | format_mfa(Module,Name,Arity)])
    end;
save_word(Fd, _Current, _Unthread, W) -> %% literals etc
    emit_string(Fd, ",\n  "),
    emit_value(Fd, W).

format_mfa(M,F,A) when is_atom(M), is_atom(F), is_integer(A) ->
    [format_atom(M),":",format_atom(F),"/",format_int(A)];
format_mfa(M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
    %% FIXME: format arguments
    [format_atom(M),":",format_atom(F),"/",format_int(length(A))].

format_int(X) ->
    integer_to_list(X).

format_atom(A) when is_atom(A) ->
    Cs = atom_to_list(A),
    case need_quote(Cs) of
	true -> [$'] ++ Cs ++ [$'];
	false -> Cs
    end.

need_quote([C|Cs]) ->
    if C >= $a, C =< $z -> need_quote_(Cs);
       true -> true
    end.
    
need_quote_([C|Cs]) ->
    if C >= $0, C =< $9 -> need_quote_(Cs);
       C >= $a, C =< $z -> need_quote_(Cs);
       C >= $A, C =< $Z -> need_quote_(Cs);
       C =:= $@ -> need_quote_(Cs);
       C =:= $_ -> need_quote_(Cs);
       true -> true
    end;
need_quote_([]) ->
    false.

%% "save" the dictionary
save_dict_words(Fd, Module, WordsName, Dict) when is_atom(Module) ->
    emit_strings(Fd, [WordsName,"() ->\n  #{\n"]),
    maps:fold(
      fun(Name, _Xt, _Acc) ->
	      emit_strings(Fd,["      <<\"", Name, "\">> => ",
			       format_mfa(Module,binary_to_atom(Name),0),
			       ",\n"])
      end, [], Dict),
    emit_string(Fd, "      <<>> => false\n"),  %% dummy clause (FIXME)
    emit_string(Fd, "  }.\n").


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
			    emit_char(Fd, ?SPACE),
			    emit_string(Fd, ModStr),
			    emit_string(Fd, Name)
		    end;
		Semis ->
		    ok;
		_ ->
		    Name = ?nf(Def),
		    emit_char(Fd, ?SPACE),
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
    emit_char(Fd, ?SPACE),
    emit_value(Fd, W).

init() ->
    Args = [list_to_binary(Arg) || Arg <- init:get_plain_arguments()],
    Argv = list_to_tuple(Args),
    try ffe_tty:open() of
	TTY ->
	    put(user, #user { 
			 tib  = prim_buffer:new(),
			 pad  = prim_buffer:new(),
			 argv = ?WPTR(1,Argv),
			 tty = TTY,
			 source_id = ?TERMINAL_INPUT,
			 altout = ?TERMINAL_OUTPUT })
    catch
	error:einval ->  %% assume pipe/standard io
	    put(user, #user { 
			 tib  = prim_buffer:new(),			 
			 pad  = prim_buffer:new(),
			 argv = ?WPTR(1,Argv),
			 tty = undefined,
			 source_id = ?STANDARD_INPUT,
			 altout = ?STANDARD_OUTPUT })
    end,
    ok.

get_user(Field) ->
    element(Field, get(user)).

set_user(Field, Value) ->
    U = setelement(Field, get(user), Value),
    put(user, U),
    Value.

get_state() -> 
    get_user(#user.state).
set_state(State) when is_integer(State) ->
    set_user(#user.state,State).

-spec get_tib() -> prim_buffer().
get_tib() ->
    get_user(#user.tib).
-spec set_tib(Tib::prim_buffer()) -> prim_buffer().
set_tib(Tib) when is_reference(Tib) ->
    set_user(#user.tib, Tib).

-spec get_pad() -> prim_buffer().
get_pad() ->
    get_user(#user.pad).
-spec set_pad(Pad::prim_buffer()) -> prim_buffer().
set_pad(Pad) when is_reference(Pad) ->
    set_user(#user.pad, Pad).
    
get_span() ->
    prim_buffer:size(get_tib()).
%% get_user(#user.span).
%%set_span(Size) when is_integer(Size),Size>=0 -> 
%%    set_user(#user.span,Size).

get_in() ->
    0.
%%    get_user(#user.in).
set_in(Offset) when is_integer(Offset),Offset>=0 -> 
    prim_buffer:skip(get_tib(), Offset).
%%    set_user(#user.in, Offset).

get_out() ->
    get_user(#user.out).
set_out(N) when is_integer(N),N>=0 -> 
    set_user(#user.out, N).

get_source_id() -> 
    get_user(#user.source_id).

set_source_id(Fd) ->
    set_user(#user.source_id, Fd).

%% source is tib in> @
get_source() -> 
    get_tib().
set_source(String) ->
    Tib = get_tib(),
    prim_buffer:wipe(Tib),
    prim_buffer:write(Tib, [String]).
clr_source() ->
    prim_buffer:wipe(get_tib()).

make_source(String) when is_binary(String) ->
    B = prim_buffer:new(),
    prim_buffer:write(B, [String]),
    B.

altout() -> get_user(#user.altout).

tty() -> get_user(#user.tty).

get_base() -> 
    get_user(#user.base).
set_base(Base) when is_integer(Base), Base > 1, Base =< 36 ->
    set_user(#user.base, Base).

%% csp is a list of value (stack) in this implementation
get_csp() ->
    get_user(#user.csp).
set_csp(Stack) ->
    set_user(#user.csp,Stack).

cf_push(Tag) ->  set_csp([Tag|get_csp()]).

cf_pop() ->
    case get_csp() of
	[] -> throw({-22, control_structure});
	[Tag|Stack1] ->
	    set_csp(Stack1),
	    Tag
    end.

%% cf_top() -> hd(get_csp()).
cf_reset() -> set_csp([]).

get_hsp() ->
    get_user(#user.hsp).
set_hsp(Stack) ->
    set_user(#user.hsp,Stack).

%% push a new char list onto hold "stack"
hold_begin() ->
    set_hsp([[]|get_hsp()]).

%% pop char list from hold stack and make binary
hold_end() ->
    [Hold | Stack] = get_hsp(),
    set_hsp(Stack),
    list_to_binary(Hold).

hold_char(Char) ->
    [Hold | Stack] = get_hsp(),
    set_hsp([[Char|Hold]|Stack]).

hold_number(0) ->
    hold_char($0);
hold_number(N) ->
    hold_digits(-1,N).

%% format M digits from unsigned value N in base B
%% if M<0 then format all digits until N=0
%%    M=0 stop format
%%    M>0 format more digits
hold_digits(M,N) ->
    hold_digits(M,N,get_base()).

hold_digits(0,N,_B) ->
    N;
hold_digits(M,0,_B) when M < 0 ->
    0;
hold_digits(M,N,B) ->
    Digit = N rem B,
    Char = if Digit < 10 -> Digit+$0; true -> (Digit-10)+$A end,
    hold_char(Char),
    hold_digits(M-1,N div B,B).

here() -> get_user(#user.dp).
here(Data) when is_tuple(Data) -> 
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

set_latest(Name) when is_binary(Name) ->
    set_user(#user.latest, Name).

%% Add Name to current vocabulary
define(Name,Xt) when is_function(Xt,0) ->
    set_latest(Name),
    current(maps:put(Name,Xt,current())).

%% Call FFE from erlang
call(Xt, Args) when is_function(Xt,0), is_list(Args) ->
    call_(Xt(), Args);
call(W, Args) when is_tuple(W), is_list(Args) ->
    call_(W, Args).

call_(W, Args) ->
    CFA = ?cf(W),
    R = exec_ret(),
    put(user, #user {}),
    CFA(lists:reverse(Args),[],?WPTR(?PFA,R),?WPTR(?PFA,W)).

%% FIRST VERSION in Erlang - to validate the idea
%% REWRITE in forth!
%%
quit0() ->
    clr_source(),  %% clear text input buffer
    set_state(0),
    main([]).

main(SP) ->
    Compile = is_compiling(),
    case word(?SPACE) of
	eof ->
	    SP;
	Name ->
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
		    emit_strings(altout(),[Name, " ?\n"]),
		    quit0()
	    end
    end.

%% $abc ignore base and use hex number
to_integer(<<$$,Cs/binary>>, _Base) ->
    binary_to_integer(Cs, 16);
to_integer(Cs, Base) ->
    binary_to_integer(Cs, Base).

create_word(Name,CFA) when is_binary(Name),
				is_function(CFA,4) ->
    ?CREATE_WORD(Name, CFA).
create_word(Name,CFA,PFA1) when is_binary(Name),
				is_function(CFA,4) ->
    ?CREATE_WORD(Name, CFA, PFA1).

create_imm(Name,CFA,PFA1) when is_binary(Name),
			       is_function(CFA,4) ->
    ?CREATE_IMM(Name, CFA, PFA1).

%% stub for return after exec
exec_ret() ->
    ?CREATE_WORD(<<"(ret)">>,fun ?MODULE:exec_ret/4,fun ?MODULE:ret/0).
exec_ret(_SP,_RP,_IP,_WP) ->
    erlang:display("INTERNAL ERROR\n"),
    throw({?QUIT, quit}).

exec(W, SP) ->
    CFA = ?cf(W),
    R = exec_ret(),
    try CFA(SP,[],?WPTR(?PFA,R),?WPTR(?PFA,W)) of
	SP1 -> main(SP1)
    catch
	throw:{?BYE=_Code,_Reason} ->
	    halt(0); %% ??
	throw:{?QUIT=_Code,_Reason} ->
	    quit0();
	throw:{?ABORT=_Code,_Reason} ->
	    quit0();
	throw:{?ABORTQ=_Code,_Reason} ->
	    quit0();
	throw:{?UNDEF=Code,Reason} ->
	    ffe_tio:output(?STANDARD_OUTPUT, io_lib:format("~w : ~p", [Code,Reason])),
	    quit0();
	throw:{Code,Reason} ->
	    ffe_tio:output(?STANDARD_OUTPUT, io_lib:format("~w : ~p", [Code,Reason])),
	    quit0();
	?EXCEPTION(error,{case_clause,SP0},_StackTrace) when is_list(SP0) ->
	    ffe_tio:output(?STANDARD_OUTPUT, "Stack empty\r\n"),
	    dump_stacktrace(?GET_STACK(_StackTrace)),
	    quit0();
	?EXCEPTION(error,function_clause,_StackTrace) ->
	    ffe_tio:output(?STANDARD_OUTPUT, "Stack empty\r\n"),
	    dump_stacktrace(?GET_STACK(_StackTrace)),
	    quit0();
	?EXCEPTION(error,Reason,_StackTrace) ->
	    ffe_tio:output(?STANDARD_OUTPUT, "Internal error "),
	    ffe_tio:output(?STANDARD_OUTPUT, io_lib:format("~p", [Reason])),
	    ffe_tio:output(?STANDARD_OUTPUT, [?CRNL]),
	    dump_stacktrace(?GET_STACK(_StackTrace)),
	    quit0()
    end.

?XT("evaluate",evaluate).
evaluate([U,Addr|SP],RP,IP,WP) ->
    <<String:U/binary, _/binary>> = Addr,
    Tib0 = get_tib(),
    State0  = get_state(),
    SourceID0 = get_source_id(),
    %% setup evaluation env
    set_source_id(?STRING_INPUT),
    set_source(make_source(String)),
    set_state(0),
    SP1 = main(SP),
    set_source_id(SourceID0),
    set_tib(Tib0),
    set_state(State0),
    next(SP1,RP,IP,WP).

%% stub for return after exec
cret() ->
    {0,<<"(cret)">>,fun ?MODULE:cret/4,fun ?MODULE:ret/0}.
cret(SP,_RP,_IP,_WP) ->
    case get_user(#user.exceptions) of
	[{_SP1,RP1,IP1,WP1}|Exceptions] ->
	    set_user(#user.exceptions, Exceptions),
	    next([0|SP],RP1,IP1,WP1)
    end.

?XT("catch", catch0).
catch0([Xt|SP],RP,IP,WP) ->
    Exceptions = get_user(#user.exceptions),
    set_user(#user.exceptions, [{SP,RP,IP,WP}|Exceptions]),
    W = Xt(),
    CFA = ?cf(W),
    R = cret(),
    CFA(SP,RP,?WPTR(?PFA,R),?WPTR(?PFA,W)).

?XT("throw", throw0).
throw0([N|SP],RP,IP,WP) ->
    if N =:= 0 ->
	    next(SP,RP,IP,WP);
       true ->
	    case get_user(#user.exceptions) of
		[{SP1,RP1,IP1,WP1}|Exceptions] ->
		    set_user(#user.exceptions, Exceptions),
		    next([N|SP1],RP1,IP1,WP1)
	    end
    end.

dump_stacktrace(StackTrace) ->
    dump_stacktrace(StackTrace,?MAX_STACKTRACE_DEPTH).

dump_stacktrace(_StackTrace, 0) ->
    ffe_tio:output(?STANDARD_OUTPUT, ["  ...",?CRNL]),
    ok;
dump_stacktrace([{ffe,Word,0,_}|Stack],Depth) ->
    Msg = atom_to_list(Word),
    ffe_tio:output(?STANDARD_OUTPUT, ["  ",Msg,?CRNL]),
    dump_stacktrace(Stack, Depth-1);
dump_stacktrace([{Mod,Fun,Arity,Location}|Stack],Depth) ->
    Info =
	case proplists:get_value(file, Location, undefined) of
	    undefined -> "";
	    File ->
		Line = proplists:get_value(line, Location, 0),
		LineInfo = integer_to_list(Line),
		[File,":",LineInfo," "]
	end,
    Msg = [Info, format_mfa(Mod,Fun,Arity)],
    ffe_tio:output(?STANDARD_OUTPUT, ["  ",Msg,?CRNL]),
    dump_stacktrace(Stack,Depth-1);
dump_stacktrace([],_Depth) ->
    ok.

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
word(C) ->
    parse_(C,true).
parse(C) ->
    parse_(C,false).

parse_(C, DoSkip) ->
    parse_(get_tib(), C, DoSkip).

parse_(Buffer, C, DoSkip) ->
    case prim_buffer:find_byte_index(Buffer, C) of
	{ok,0} when DoSkip ->
	    prim_buffer:skip(Buffer, 1),
	    parse_(Buffer, C, DoSkip);
	{ok,N} ->
	    Result = prim_buffer:read(Buffer, N),
	    prim_buffer:skip(Buffer, 1), %% skip C character
	    Result;
	not_found ->
	    case prim_buffer:size(Buffer) of
		0 ->
		    case refill(Buffer, get_source_id()) of
			false -> eof;
			true -> parse_(Buffer, C, DoSkip)
		    end;
		N ->
		    prim_buffer:read(Buffer, N)
	    end
    end.

char_() ->
    char_(get_tib()).

char_(Buffer) ->
    case prim_buffer:size(Buffer) of
	0 ->
	    case refill(Buffer, get_source_id()) of
		false -> eof;
		true -> char_(Buffer)
	    end;
	_N ->
	    %% fixme: utf8
	    <<Char>> = prim_buffer:read(Buffer, 1),
	    Char
    end.

?XT("source", source).
source(SP,RP,IP,WP) ->
    T = get_tib(),
    N = prim_buffer:size(T),
    next([N,T|SP],RP,IP,WP).

?XT("refill", refill).
refill(SP,RP,IP,WP) ->
    R = refill_tib(),
    next([?BOOL(R)|SP],RP,IP,WP).

?XT("accept", accept).
accept([N1,Buffer|SP],RP,IP,WP) ->
    N2 = accept_buffer(Buffer,N1),
    next([N2|SP],RP,IP,WP).

?XT("pad", pad).
pad(SP,RP,IP,WP) ->
    Pad = get_pad(),
    next([Pad|SP],RP,IP,WP).

?XT("number", number).
number([U,Buffer|SP],RP,IP,WP) ->
    Data = prim_buffer:read(Buffer, U),
    try binary_to_integer(Data) of
	Int -> next([Int|SP],RP,IP,WP)
    catch
	error:_ ->
	    throw({?ARGUMENT_MISMATCH, argument_mismatch})
    end.

?XT("save-input", save_input).
save_input(SP,RP,IP,WP) ->
    %% file position?
    %% FIXME
    next(SP,RP,IP,WP).

?XT("restore-input", restore_input).
restore_input(SP,RP,IP,WP) ->
    %% file position?
    %% FIXME
    next(SP,RP,IP,WP).

%% read a line and put it in TIB 
refill_tib() ->
    refill(get_tib(), get_source_id()).

refill(_, ?STRING_INPUT) ->
    false;
refill(Buffer, SourceID) ->
    ALTOUT = altout(),
    case is_compiling() of
	false when SourceID =:= ?TERMINAL_INPUT, 
		   ALTOUT =:= ?TERMINAL_OUTPUT ->
	    emit_string(ALTOUT, " ok\r\n");
	_ ->
	    ok
    end,
    case ffe_tio:get_line(SourceID) of
	eof ->
	    prim_buffer:wipe(Buffer),
	    %% set_source(<<>>),
	    false;
	Data when is_binary(Data) ->
	    set_out(0), %%? ok? or terminal only?
	    prim_buffer:wipe(Buffer),
	    prim_buffer:write(Buffer, [Data]),
	    %% set_source(Data),
	    true
    end.

accept_buffer(Buffer, Max) ->
    prim_buffer:wipe(Buffer),
    accept_buffer_(get_source_id(),Buffer,Max).

accept_buffer_(SourceID, Buffer, Max) ->
    case ffe_tio:get_line(SourceID) of
	eof -> 
	    0;
	Data when byte_size(Data) =< Max ->
	    io:format("write data ~p to ~p\r\n", [[Data, <<"\s">>],Buffer]),
	    prim_buffer:write(Buffer, [Data,<<"\s">>]),
	    byte_size(Data);
	<<Data:Max/binary,_/binary>> ->
	    prim_buffer:write(Buffer, [Data,<<"\s">>]),
	    Max
    end.
    
expand(RevLine) ->
    %% match dictionary and collect all prefix matches
    Match = collect_until(RevLine, ?SPACE, []),
    case match_dicts(Match, [ffe:current() | ffe:forth()], []) of
	[] ->
	    {no, <<"">>, []};
	[OneMatch] -> 
	    Insert = remove_prefix(OneMatch, Match),
	    {yes, <<Insert/binary,?SPACE>>, []};
	MultipleMatches=[FirstMatch|RestOfMatches] ->
	    CommonMatch =
		lists:foldl(
		  fun(String, Prefix) ->
			  common_prefix(Prefix, String)
		  end, FirstMatch, RestOfMatches),
	    Insert = remove_prefix(CommonMatch, Match),
	    {yes, Insert, MultipleMatches}
    end.

match_dicts(Match, [Dict|Ds], Acc) ->    
    Acc1 = match_dict(Match, Dict, Acc),
    match_dicts(Match, Ds, Acc1);
match_dicts(_Match, [], Acc) -> 
    Acc.

match_dict(Match, Dict, Acc0) ->
    MatchLen = ?name_len(Match),
    maps:fold(
      fun(Name, _Xt, Acc) ->
	      case Name of
		  <<Match:MatchLen/binary, _/binary>> ->
		      [Name|Acc];
		  _ ->
		      Acc
	      end
      end, Acc0, Dict).

remove_prefix(String, Prefix) ->
    PrefixLen = ?name_len(Prefix),
    case String of
	<<Prefix:PrefixLen/binary, String1/binary>> ->
	    String1
    end.

%% extract common prefix (utf8?)
common_prefix(String1, String2) ->
    common_prefix(String1, String2, []).

common_prefix(<<C,Cs/binary>>, <<C,Ds/binary>>, Acc) ->
    common_prefix(Cs,Ds,[C|Acc]);
common_prefix(_Cs,_Ds,Acc) -> 
    list_to_binary(lists:reverse(Acc)).

%% collect input chars until C is matched
collect_until([C|_Cs], C, Acc) ->
    list_to_binary(Acc);  %% utf8?
collect_until([], _C, Acc) ->
    list_to_binary(Acc);  %% utf8
collect_until([C|Cs], Char, Acc) ->
    collect_until(Cs, Char, [C|Acc]).

%% FIXME use "real" output routine
format_word_list(_Out, []) ->
    ok;
format_word_list(Out, WordNameList) ->
    emit_chars(Out, [?CRNL]),
    Width = lists:max([?name_len(WordName) || WordName <- WordNameList])+1,
    format_lines(Out, WordNameList, 76, 76, Width).

format_lines(Out, [Word|WordNameList], LineLength, Remain, Width) ->
    if Remain < 0; Remain - Width < 0 ->
	    emit_chars(Out, [?CRNL]),
	    N = emit_counted_string(Out, Word, Width),
	    format_lines(Out, WordNameList, LineLength, LineLength-N, Width);
       true ->
	    N = emit_counted_string(Out, Word, Width),
	    format_lines(Out, WordNameList, LineLength, Remain-N, Width)
    end;
format_lines(Out, [], LineLength, Remain, _Width) ->
    if LineLength =/= Remain ->
	    emit_chars(Out, [?CRNL]);
       true ->
	    ok
    end.

emit_counted_string(Fd, WordName, Width) ->
    N = ?name_len(WordName),
    if N < Width ->
	    ffe_tio:output(Fd, WordName),
	    ffe_tio:output(Fd, lists:duplicate(Width-N,?SPACE)),
	    set_out(get_out()+Width),
	    Width;
       true ->
	    ffe_tio:output(Fd, WordName),
	    set_out(get_out()+N),
	    N
    end.

%%  ( ch c-addr len -- c-addr n1 n2 n3 )
%%  n1 = offset to first none ch char (word start)
%%  n2 = offset to last char in word (word stop)
%%  n3 = length of enclosed data
enclose(Ch,Data,DoSkip) ->
    Len = byte_size(Data),
    N1 = if DoSkip -> skip(Ch, Data, 0); true -> 0 end,
    N2 = take(Ch, Data, N1),
    if N2 < Len -> [N1,N2,(N2-N1)];
       true -> [N1,N2,(N2-N1)]
    end.

skip(Ch, Data, Offs) ->
    case Data of
	<<_:Offs/binary,Ch,_/binary>> ->
	    skip(Ch,Data,Offs+1);
	_ -> Offs
    end.

take(Ch, Data, Offs) ->
    case Data of
	<<_:Offs/binary,Ch,_/binary>> ->
	    Offs;
	<<_:Offs/binary, _/binary>> ->
	    take(Ch,Data,Offs+1);
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
      ?WORD("argv", argv),

      ?WORD("0",          zero),
      ?WORD("1",          one),
      ?WORD("-1",         minus_one),
      
      ?WORD("create",     create),
      ?WORD("does>",      does),
      
      %% meta words - pre compile
      ?WORD("constant",   compile_constant),
      ?WORD("variable",   compile_variable),
      ?WORD("value",      compile_value),
      ?WORD("user",       compile_user),
      ?WORD("defer",      compile_defer),
      ?WORD("to",         to),
      ?WORD("is",         is),
      ?WORD("do",         compile_do),
      ?WORD("?do",        compile_qdo),
      ?WORD("loop",       compile_loop),
      ?WORD("if",         compile_if),
      ?WORD("then",       compile_then),
      ?WORD("else",       compile_else),
      ?WORD("immediate",  immediate),
      
      ?WORD("\\",         backslash),
      ?WORD("(",          paren),
      ?WORD("(lit)",      lit),
      ?WORD("literal",    literal),
      ?WORD(",\"",        comma_quote),
      ?WORD("s\"",        s_quote),
      ?WORD("c\"",        c_quote),

      ?WORD("char",       care),

      ?WORD("refill",     refill),
      ?WORD("source",     source),
      ?WORD("environment?", environment_query),
      ?WORD("pad",        pad),
      ?WORD("accept",     accept),
      ?WORD("number",     number),
      ?WORD("evaluate",   evaluate),
      ?WORD("catch",      catch0),
      ?WORD("throw",      throw0),

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
    CFA(SP,RP,?WPTR(IP+1,Code),?WPTR(?PFA,W)).

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
    Name = word(?SPACE),
    Def = create_word(Name, fun ?MODULE:docon/4, Value), 
    define(Name, fun() -> Def end),
    next(SP,RP,IP,WP).

%% VARIABLE
?XT("variable", compile_variable).
compile_variable(SP,RP,IP,WP) ->
    Name = word(?SPACE),
    Var = make_variable_ref(),
    Def = create_word(Name, fun ?MODULE:dovar/4, Var),
    define(Name, fun() -> Def end),
    next(SP,RP,IP,WP).

%% VALUE
?XT("value", compile_value).
compile_value([Value|SP],RP,IP,WP) ->
    Name = word(?SPACE),
    Var = make_variable_ref(),
    set_value(Var, Value),
    Def = create_word(Name, fun ?MODULE:doval/4, Var),
    define(Name, fun() -> Def end),
    next(SP,RP,IP,WP).

%% DEFER
?XT("defer", compile_defer).
compile_defer(SP,RP,IP,WP) ->
    Name = word(?SPACE),
    Var = make_variable_ref(),
    Def = create_word(Name, fun ?MODULE:doexec/4, Var),
    define(Name, fun() -> Def end),
    next(SP,RP,IP,WP).

?XT("is", is).
is([Xt|SP],RP,IP,Code) ->
    Name = word(?SPACE),
    case find_word_(Name) of
	{_, Yt} ->
	    Var = element(?PFA, Yt()),
	    set_value(Var, Xt),
	    next(SP,RP,IP,Code);
	false ->
	    throw({?UNDEF, Name})
    end.

make_variable_ref() ->
    {var, erlang:unique_integer([])}.

compile_user() ->
    { 0, <<"user">>, fun ?MODULE:compile_user/4 }.
compile_user([Value|SP],RP,IP,WP) ->
    Name = word(?SPACE),
    Def = create_word(Name, fun ?MODULE:dousr/4, Value),
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

?IXT("immediate", immediate).
immediate(SP,RP,IP,WP) ->
    Latest = get_user(#user.latest),
    case maps:get(Latest, current(), false) of
	false ->
	    %% fixme: abort?
	    next(SP,RP,IP,WP);
	Xt ->
	    W = Xt(),
	    W1 = ?set_ff(W, ?ff(W) bor ?IMMEDIATE),
	    define(Latest,fun() -> W1 end),
	    next(SP,RP,IP,WP)
    end.

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
    Name = word(?SPACE),
    Does0 = create_word(Name,fun ?MODULE:does0/4, 0),
    here(Does0),
    set_latest(Name),
    cf_push(?CF_CREATE),
    next(SP, RP, IP, WP).

?XT("does>", does).
does(SP,RP,IP,WP) ->
    case get_csp() of
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
	    set_csp(Csp),  %% pop control stack
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

%%?XT("base", base).
%%base(SP,RP,IP,Code) ->
%%    next([{user,#user.base}|SP],RP,IP,Code).
?XUSR("base", base, #user.base).
?XUSR("argv", argv, #user.argv).
?XUSR(">in", to_in, #user.in).


?XT("to", to).
to([Value|SP],RP,IP,Code) ->
    Name = word(?SPACE),
    case find_word_(Name) of
	{_, Xt} ->
	    Var = element(?PFA, Xt()),
	    set_value(Var, Value),
	    next(SP,RP,IP,Code);
	false ->
	    throw({?UNDEF, Name})
    end.

?XT("leave", leave).
leave(SP,[_,RP=[Limit|_]],IP,WP) ->
    next(SP,[Limit|RP],IP,WP).

?XT("ret", ret).
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

?XT("execut", execut).
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

?XT("(docon)", docon).
docon(SP,RP,IP,WP0=?WPTR(W,WP)) ->
    next([element(W,WP)|SP],RP,IP,WP0).

?XT("(dousr)", dousr).
dousr(SP,RP,IP,WP0=?WPTR(W,WP)) ->
    next([{user,element(W,WP)}|SP],RP,IP,WP0).

?XT("(dovar)", dovar).
dovar(SP,RP,IP,WP0=?WPTR(W,WP)) ->
    next([element(W,WP)|SP],RP,IP,WP0).

?XT("(doval)", doval).
doval(SP,RP,IP,WP0=?WPTR(W,WP)) ->
    next([get_value(element(W,WP))|SP],RP,IP,WP0).

?XT("(doexec)", doexec).
doexec(SP,RP,IP,WP0=?WPTR(Wi,WP)) ->
    case get_value(element(Wi,WP)) of
	0 ->
	    next(SP,RP,IP,WP0);
	Xt ->
	    W = Xt(),            %% tuple word
	    CFA = ?cf(W),        %% get code field
	    CFA(SP,RP,IP,?WPTR(?PFA,W))
    end.

%% default does code with no offset
does0(SP,RP,?WPTR(IP,Code),WP=?WPTR(Pos,Wf)) ->
    next([?WPTR(Pos+1,Wf)|SP],RP,?WPTR(IP+1,Code),WP).

%% does with offset to does> code
does1() ->
    { 0, <<"(does>)">>, fun ?MODULE:does1/4 }.
does1(SP,RP,IP,WP=?WPTR(Pos,Wft)) ->
    ?WPTR(XPos,Xt) = element(Pos, Wft),
    next([?WPTR(Pos+1,Wft)|SP],[IP|RP],?WPTR(XPos,Xt()),WP).

semis() ->    
    { 0, "(semis)", fun ?MODULE:semis/4 }.
semis(SP,[IP|RP],_IP,WP) ->
    %% special treat when reach ';' while doing create and 
    %% does> is not found
    case get_csp() of
	[?CF_CREATE|Csp] ->
	    Def = here(),
	    Name = element(?NFA, Def),
	    define(Name, fun() -> Def end),
	    here({}),  %% clear defintion area
	    set_csp(Csp),  %% pop control stack
	    %% set_state(0),
	    next(SP,RP,IP,WP);
	_ ->
	    next(SP,RP,IP,WP)
    end.

?XT("branch", branch).
branch(SP,RP,?WPTR(IP,Code),WP) ->
    next(SP,RP,?WPTR(IP+element(IP,Code),Code),WP).

?XT("0branch", zbranch).
zbranch([0|SP],RP,?WPTR(IP,Code),WP) ->
    Offset = element(IP,Code),
    next(SP,RP,?WPTR(IP+Offset,Code),WP);
zbranch([_|SP],RP,?WPTR(IP,Code),WP) ->
    next(SP,RP,?WPTR(IP+1,Code),WP).

%% char
?XT("char", care).
care(SP,RP,IP,WP) ->
    Char = char_(),
    next([Char|SP],RP,IP,WP).

?IXT("\\", backslash).
backslash(SP,RP,IP,WP) ->
    set_in(get_span()),  %% skip all characters in input buffer
    next(SP,RP,IP,WP).

?IXT("(", paren). 
paren(SP,RP,IP,WP) ->
    parse($)),       %% skip until ')'
    next(SP,RP,IP,WP).

%% compile counted string (fixme? allow max 255 chars)
?IXT(",\"", comma_quote).
comma_quote(SP,RP,IP,WP) ->
    compile_only(),
    String = parse($"),
    comma_(fun ffe:lit/0),
    comma_(String),
    next(SP,RP,IP,WP).

%% compile: counted string
?IXT("c\"", c_quote).
c_quote(SP,RP,IP,WP) ->
    compile_only(),
    String = parse($"),
    comma_(fun ffe:lit/0),
    comma_(String),
    next(SP,RP,IP,WP).

%% compile: len/string interpret: push len/string
?IXT("s\"", s_quote).
s_quote(SP,RP,IP,WP) ->
    String = parse($"),
    case is_compiling() of
	true ->
	    comma_(fun ffe:lit/0),
	    comma_(String),
	    comma_(fun ffe:lit/0),
	    comma_(byte_size(String)),
	    next(SP,RP,IP,WP);
	false ->
	    next([byte_size(String),String|SP],RP,IP,WP)
    end.

%% output a word as erlang code - fixme only allow colon defs!
?XT("show", show_word).
show_word(SP,RP,IP,WP) ->
    Name = word(?SPACE),
    case find_word_(Name) of
	{_, Xt} ->
	    show_def(altout(), Name, ?UNTHREAD_NONE, Xt);
	false ->
	    throw({?UNDEF, Name})
    end,
    next(SP,RP,IP,WP).

%% unthread a word
?XT("unthread", unthread_word).
unthread_word(SP,RP,IP,WP) ->
    Name = word(?SPACE),
    case find_word_(Name) of
	{_, Xt} ->
	    save_def(altout(), ffe, Name, ?UNTHREAD_ALL, Xt);
	false ->
	    throw({?UNDEF, Name})
    end,
    next(SP,RP,IP,WP).

%% Remove word from current dictionary
%% Remember that words that refer to this word will 
%% continue to work in interactive mode but will not compile!
?XT("remove", remove_word).
remove_word(SP,RP,IP,WP) ->
    Name = word(?SPACE),
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

emit_value(Value) ->
    emit_value(altout(), Value).
emit_value(Fd, Value) ->
    if is_integer(Value) ->
	    emit_string(Fd,integer_to_list(Value, get_base()));
       is_float(Value) ->
	    emit_string(Fd,io_lib_format:fwrite_g(Value));
       is_binary(Value) ->
	    emit_strings(Fd,["\"",Value,"\""]);
       is_pid(Value) ->
	    emit_string(Fd,pid_to_list(Value));
       true ->
	    %% hmm recursivly display integers in base()!
	    %% FIXME: remove all use of io_lib and io
	    emit_string(Fd,lists:flatten(io_lib:format("~p",[Value])))
    end.    

    
emit_char(Char) ->
    emit_char(altout(), Char).

emit_char(Fd, Char) ->
    if Char =:= $\r ->
	    ffe_tio:output(Fd,[$\r]),
	    set_out(0);
       Char =:= $\n ->
	    ffe_tio:output(Fd,[$\n]);
       true ->
	    ffe_tio:output(Fd,[Char]),
	    set_out(get_out()+1)
    end.

emit_chars(Chars) ->
    emit_chars(altout(), Chars).

emit_chars(Fd, Binary) when is_binary(Binary) ->
    emit_chars_(Fd, binary_to_list(Binary));
emit_chars(Fd, Chars) when is_list(Chars) ->
    emit_chars_(Fd, Chars).

emit_chars_(_Fd, []) ->
    ok;
emit_chars_(Fd, [C|Cs]) ->
    emit_char(Fd, C),
    emit_chars_(Fd, Cs).

emit_nchars(N, Chars) ->
    emit_chars(altout(), N, Chars).

emit_chars(Fd, N, Binary) when is_binary(Binary) ->
    emit_nchars_(Fd, N, binary_to_list(Binary));
emit_chars(Fd, N, CharList) when is_list(CharList) ->
    emit_nchars_(Fd, N, CharList).

emit_nchars_(_Fd, _N, []) ->  ok;
emit_nchars_(_Fd, 0, _Cs) ->   ok;
emit_nchars_(Fd, N, [C|Cs]) ->
    emit_char(Fd, C),
    emit_nchars_(Fd, N-1, Cs).

emit_strings(ListOfStrings) ->
    emit_strings(altout(),ListOfStrings).
emit_strings(Fd,ListOfStrings) when is_list(ListOfStrings) ->
    [emit_string(Fd,String) || String <- ListOfStrings].

emit_string(String) ->
    emit_string(altout(), String).
emit_string(Fd, Binary) when is_binary(Binary) ->
    emit_string_(Fd, binary_to_list(Binary));
emit_string(Fd,Chars) when is_list(Chars) ->
    emit_string_(Fd, Chars).

emit_string_(_Fd, []) ->
    ok;
emit_string_(Fd, Cs) ->
    case collect_line(Cs, 0, []) of
	{true,_Len,Chars,Cs1} ->
	    ffe_tio:output(Fd, Chars),
	    ffe_tio:output(Fd, [?CR,?NL]),
	    set_out(0),
	    emit_string_(Fd, Cs1);
	{false,Len,Chars,Cs1} ->
	    ffe_tio:output(Fd, Chars),
	    set_out(get_out()+Len),
	    emit_string_(Fd, Cs1)
    end.

collect_line([$\n|Cs], N, Acc) ->
    {true, N, lists:reverse(Acc), Cs};
collect_line([C|Cs], N, Acc) ->
    collect_line(Cs, N+1, [C|Acc]);
collect_line([], N, Acc) ->
    {false, N, lists:reverse(Acc), []}.

-include("core.i").
-include("core_ext.i").
-include("common.i").
%%-include("search.i").
%%-include("string.i").
-include("tools.i").
-include("tools_ext.i").
-include("file.i").
-include("facility.i").

-ifdef(INCLUDE_STRAP).
-include("strap.i").
-else.
strap_words() ->
    #{}.
-endif.

?XCON("0", zero, 0).
?XCON("1", one,  1).
?XCON("-1", minus_one,  -1).

-ifdef(__CORE__).
-define(CORE_PRESENT, ?TRUE).
-else.
-define(CORE_PRESENT, ?FALSE).
-endif.

-ifdef(__CORE_EXT__).
-define(CORE_EXT_PRESENT, ?TRUE).
-else.
-define(CORE_EXT_PRESENT, ?FALSE).
-endif.

-ifdef(__SEARCH__).
-define(SEARCH_PRESENT, ?TRUE).
-else.
-define(SEARCH_PRESENT, ?FALSE).
-endif.

-ifdef(__STRING__).
-define(STRING_PRESENT, ?TRUE).
-else.
-define(STRING_PRESENT, ?FALSE).
-endif.

-ifdef(__TOOLS__).
-define(TOOLS_PRESENT, ?TRUE).
-else.
-define(TOOLS_PRESENT, ?FALSE).
-endif.

-ifdef(__TOOLS_EXT__).
-define(TOOLS_EXT_PRESENT, ?TRUE).
-else.
-define(TOOLS_EXT_PRESENT, ?FALSE).
-endif.

-ifdef(__FILE__).
-define(FILE_PRESENT, ?TRUE).
-else.
-define(FILE_PRESENT, ?FALSE).
-endif.

-ifdef(__FACILITY__).
-define(FACILITY_PRESENT, ?TRUE).
-else.
-define(FACILITY_PRESENT, ?FALSE).
-endif.

value(M,F,Default) ->
    case erlang:function_exported(M,F,0) of
	true ->
	    DoCon = fun ffe:docon/4,
	    try M:F() of
		{_Flags,_Name, DoCon, V} -> V;
		_ -> Default
	    catch
		error:_ ->
		    Default
	    end;
	false ->
	    Default
    end.

has(M,F) ->
    value(M,F,?FALSE).

?XT("environment?", environment_query).
environment_query([U,CAddr|SP],RP,IP,WP) ->    
    <<String:U/binary,_/binary>> = CAddr,
    case environment_lookup(String) of
	undefined ->
	    next([?FALSE|SP],RP,IP,WP);
	Value ->
	    next([?TRUE,Value|SP],RP,IP,WP)
    end.

environment_lookup(String) ->
    case String of
	<<"/counted-string">> -> 65535;
	<<"/hold">> -> 135;
	<<"/pad">> -> 1024;
	<<"block">> -> ?FALSE;
	<<"block-ext">> -> ?FALSE;
	<<"core">> -> ?CORE_PRESENT;
	<<"core-ext">> -> ?CORE_EXT_PRESENT;
	<<"double">> -> ?FALSE;
	<<"double-ext">> -> ?FALSE;
	<<"exception">> -> ?FALSE;
	<<"exception-ext">> -> ?FALSE;
	<<"facility">> -> ?FACILITY_PRESENT;
	<<"facility-ext">> -> ?TRUE;
	<<"file">> -> ?FILE_PRESENT;
	<<"file-ext">> -> ?TRUE;
	<<"floating">> -> has(floating, has_floating);
	<<"floating-ext">> -> has(floating_ext, has_floating_ext);
	<<"floating-stack">> -> has(floating, has_floating_stack);
	<<"floored">> -> ?TRUE;  %% fixme
	<<"max-char">> -> 255;
	<<"max-d">> -> (1 bsl 1023);
	<<"max-float">> -> value(floating, max_float, 0.0);
	<<"max-n">> -> (1 bsl 511);
	<<"max-u">> -> (1 bsl 512)-1;
	<<"max-ud">> -> (1 bsl 1024);
	<<"memory-alloc">> -> ?FALSE;
	<<"return-stack-cells">> -> 4096;
	<<"stack-cells">> -> 4096;
	<<"search-order">> -> ?FALSE;
	<<"search-order-ext">> -> ?FALSE;
	<<"string">> -> ?STRING_PRESENT;
	<<"tools">> -> ?TOOLS_PRESENT;
	<<"tools-ext">> -> ?TOOLS_EXT_PRESENT;
	<<"wordlists">> -> 100;
	_ -> undefined
    end.

