%%
%% Macros for primitives
%%
-ifndef(__FFE_HRL__).
-define(__FFE_HRL__, true).

-define(BYE,       bye).    %% fixme
-define(STACK_OVERFLOW,  -3).
-define(STACK_UNDERFLOW, -4).    %% case clause?
-define(ARITH,           -10).   %% arith
-define(UNDEF,           -13).   %% undefined word
-define(QUIT,            -56).
-define(INTRRUPT,        -28).   %% user interrupt

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

-define(name_len(Addr), byte_size((Addr))).


-define(XPORT(W), -export([W/0])).
-define(EXPORT(W), -export([W/0, W/4])).
-define(WORD(W,Xt), <<W>> => fun ?MODULE:Xt/0).
-define(XT(W), W() -> {0, <<??W>>, fun ?MODULE:W/4 }).
-define(XT(Nm,Cf), Cf() -> {0, <<Nm>>, fun ?MODULE:Cf/4 }).
-define(IXT(W),      W() -> {?IMMEDIATE, <<??W>>, fun ?MODULE:W/4 }).
-define(IXT(Nm,Cf), Cf() -> {?IMMEDIATE, <<Nm>>, fun ?MODULE:Cf/4 }).
-define(XCON(Nm,Fn,V), Fn() -> {0, <<Nm>>, fun ffe:docon/4, (V) }).
-define(XUSR(Nm,Fn,V), Fn() -> {0, <<Nm>>, fun ffe:dousr/4, (V) }).

-define(TRUE, -1).
-define(FALSE, 0).
-define(BOOL(X), if (X) -> ?TRUE; true -> ?FALSE end).

-define(STRING_INPUT,     -1).  %% evaluate

-define(TERMINAL_INPUT,   0).
-define(TERMINAL_OUTPUT,  0).

-define(STANDARD_INPUT,   1).
-define(STANDARD_OUTPUT,  1).

%% .. then files (reserve 2 for now)


%% Field pointer, reference into PFA
%% -define(WPTR(Offs,W), [(Offs)|(W)]).
%% debug version
-define(WPTR(Offs,W), {wptr,(Offs),(W)}).

-define(FIXME(), erlang:display_string("FIXME\n")).

-define(BELL,  $\^g).
-define(TAB,   $\t).
-define(CR,    $\r).
-define(NL,    $\n).
-define(CRNL,  $\r,$\n).
-define(TILDE, $~).
-define(SPACE, $\s).
-define(ESCAPE, $\e).
-define(BS, $\b).

-endif.
