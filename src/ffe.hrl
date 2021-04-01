%%
%% Macros for primitives
%%
-ifndef(__FFE_HRL__).
-define(__FFE_HRL__, true).

-define(BYE,       bye).    %% fixme
-define(ABORT,           -1).
-define(ABORTQ,          -2).

-define(ERR_STACK_OVERFLOW,  -3).
-define(ERR_STACK_UNDERFLOW, -4).    %% case clause?
-define(ERR_DIVZ,           -10).
-define(ERR_ARGTYPE,         -12).
-define(ERR_UNDEF,           -13).   %% undefined word
-define(ERR_COMPILE_ONLY,    -14).
-define(ERR_CONTROL_MISMATCH, -22).
-define(ERR_FILEPOS,           -36).
-define(ERR_FILEIO,            -37).
-define(ERR_FILENOENT,         -38).
-define(ERR_UNEXPECTEDEOF,     -39).
-define(ERR_INTERRUPT,       -28).   %% user interrupt

-define(QUIT,            -56).

%%
%% Forth word layout:
%% { Flags :: ?SMUDGE | ?IMMEDIATE
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

-define(set_ff(W, Flags), setelement(?FFA, (W), (Flags))).
-define(set_nf(W, Name), setelement(?NFA, (W), (Name))).
-define(set_cf(W, Code), setelement(?CFA, (W), (Code))).
-define(set_pf(I,W,Param), setelement(?PFA+(I)-1, (W), (Param))). %% I=1...

-define(name_len(Addr), byte_size((Addr))).

-define(CREATE_WORD(Name,CFA), {0, (Name), (CFA)}).
-define(CREATE_WORD(Name,CFA,PFA1), {0, (Name), (CFA), (PFA1)}).

-define(CREATE_IMM(Name,CFA), {?IMMEDIATE, (Name), (CFA)}).
-define(CREATE_IMM(Name,CFA,PFA1), {?IMMEDIATE, (Name), (CFA), (PFA1)}).

-define(XPORT(W), -export([W/0])).
-define(EXPORT(W), -export([W/0, W/4])).
-define(WORD(W,Xt), <<W>> => fun ?MODULE:Xt/0).
-define(XT(W), W() -> ?CREATE_WORD(<<??W>>, fun ?MODULE:W/4)).
-define(XT(Nm,Cf), Cf() -> ?CREATE_WORD(<<Nm>>, fun ?MODULE:Cf/4)).
-define(IXT(W),      W() -> ?CREATE_IMM(<<??W>>, fun ?MODULE:W/4)).
-define(IXT(Nm,Cf), Cf() -> ?CREATE_IMM(<<Nm>>, fun ?MODULE:Cf/4)).
-define(XCON(Nm,Fn,V), Fn() -> ?CREATE_WORD(<<Nm>>, fun ffe:docon/4, (V))).
-define(XUSR(Nm,Fn,V), Fn() -> ?CREATE_WORD(<<Nm>>, fun ffe:dousr/4, (V))).

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
-define(BL,    $\s).
-define(SPACE, $\s).
-define(ESCAPE, $\e).
-define(BS, $\b).
-define(Q, $\").

-endif.
