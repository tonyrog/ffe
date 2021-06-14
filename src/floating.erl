%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    FFE floating support
%%% @end
%%% Created : 19 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(floating).

-include("ffe.hrl").

-export([words/0]).
-export([format/1]).
?EXPORT('n>f').  %% no standard 
?EXPORT('f>n').
?EXPORT(fdrop).
?EXPORT(fdup).
?EXPORT(fover).
?EXPORT(frot).
?EXPORT(fswap).
?EXPORT(fdepth).
?EXPORT('f+').
?EXPORT('f-').
?EXPORT('f*').
?EXPORT('f/').
?EXPORT('f**').
?EXPORT(fabs).
?EXPORT(ffloor).
?EXPORT(fmax).
?EXPORT(fmin).
?EXPORT(fnegate).
?EXPORT(fround).
?EXPORT(fsqrt).
?EXPORT('f0<').
?EXPORT('f0=').
?EXPORT('f<').
?EXPORT(floats).
?EXPORT(float_plus).
?EXPORT(to_float).
%% FALIGN
%% FALIGNED
%% FCONSTANT
%% FLITERAL
?XPORT(has_floating).
?XPORT(has_floating_stack).
?XPORT(max_float).

?XCON("has-floating", has_floating, ?TRUE).
?XCON("has-floating-stack", has_floating_stack, ?FALSE).
?XCON("max-float", max_float, 1.797693e+308).

-define(next(SP,RP,IP,WP), ffe:next((SP),(RP),(IP),(WP))).

?XT(fdrop).
fdrop(SP,RP,IP,WP) ->
    [_|SP1] = SP,
    ?next(SP1,RP,IP,WP).    

?XT(fdup).
fdup(SP,RP,IP,WP) -> 
    [A|_] = SP,
    ?next([A|SP],RP,IP,WP).

?XT(fover).
fover(SP,RP,IP,WP) ->
    [_,B|_] = SP,
    ?next([B|SP],RP,IP,WP).

?XT(frot).
frot(SP,RP,IP,WP) -> 
    [A,B,C|SP1] = SP,
    ?next([C,A,B|SP1],RP,IP,WP).

?XT(fswap).
fswap(SP,RP,IP,WP) ->
    [B,A|SP1] = SP,
    ?next([A,B|SP1],RP,IP,WP).

?XT(fdepth).
fdepth(SP,RP,IP,WP) ->
    ?next([length(SP)|SP],RP,IP,WP).

?XT('n>f').    
'n>f'([N|SP],RP,IP,WP) -> ?next([float(N)|SP],RP,IP,WP).

'f>n'() ->
    { 0, <<"f>n">>, fun ?MODULE:'f>n'/4 }.
'f>n'([F|SP],RP,IP,WP) -> ?next([trunc(F)|SP],RP,IP,WP).

?XT('f+').
'f+'([A,B|SP],RP,IP,WP) -> ?next([B+A|SP],RP,IP,WP).

'f-'() ->
    { 0, <<"f-">>, fun ?MODULE:'f-'/4 }.
'f-'([A,B|SP],RP,IP,WP) -> ?next([B-A|SP],RP,IP,WP).

'f*'() ->
    { 0, <<"f*">>, fun ?MODULE:'f*'/4 }.
'f*'([A,B|SP],RP,IP,WP) -> ?next([B*A|SP],RP,IP,WP).

'f/'() ->
    { 0, <<"f/">>, fun ?MODULE:'f/'/4 }.
'f/'([A,B|SP],RP,IP,WP) -> ?next([B/A|SP],RP,IP,WP).

'f**'() ->
    { 0, <<"f**">>, fun ?MODULE:'f**'/4 }.
'f**'([A,B|SP],RP,IP,WP) -> ?next([math:pow(B,A)|SP],RP,IP,WP).

'fabs'() ->
    { 0, <<"fabs">>, fun ?MODULE:'fabs'/4 }.
'fabs'([A|SP],RP,IP,WP) -> ?next([abs(A)|SP],RP,IP,WP).

'ffloor'() ->
    { 0, <<"ffloor">>, fun ?MODULE:'ffloor'/4 }.
'ffloor'([A|SP],RP,IP,WP) -> ?next([math:floor(A)|SP],RP,IP,WP).

'fmin'() ->
    { 0, <<"fmin">>, fun ?MODULE:'fmin'/4 }.
'fmin'([A,B|SP],RP,IP,WP) -> ?next([min(B,A)|SP],RP,IP,WP).

'fmax'() ->
    { 0, <<"fmax">>, fun ?MODULE:'fmax'/4 }.
'fmax'([A,B|SP],RP,IP,WP) -> ?next([max(B,A)|SP],RP,IP,WP).

'fnegate'() ->
    { 0, <<"fnegate">>, fun ?MODULE:'fnegate'/4 }.
'fnegate'([A|SP],RP,IP,WP) -> ?next([-A|SP],RP,IP,WP).

'fround'() ->
    { 0, <<"fround">>, fun ?MODULE:'fround'/4 }.
'fround'([A|SP],RP,IP,WP) -> ?next([round(A)|SP],RP,IP,WP).

'fsqrt'() ->
    { 0, <<"fsqrt">>, fun ?MODULE:'fsqrt'/4 }.
'fsqrt'([A|SP],RP,IP,WP) -> ?next([math:sqrt(A)|SP],RP,IP,WP).

?XT('f<').
'f<'([A,B|SP],RP,IP,WP) -> ?next([?BOOL(B<A)|SP],RP,IP,WP).

?XT('f0<').
'f0<'([A|SP],RP,IP,WP) -> ?next([?BOOL(A<0)|SP],RP,IP,WP).

?XT('f0=').
'f0='([A|SP],RP,IP,WP) -> ?next([?BOOL(A =:= 0.0)|SP],RP,IP,WP).

?XT(floats).
floats([N|SP],RP,IP,WP) -> ?next([N*1|SP],RP,IP,WP).

?XT("float+", float_plus).
float_plus([Addr|SP],RP,IP,WP) -> ?next([Addr+1|SP],RP,IP,WP).

?XT(">float", to_float).
to_float([U,Addr|SP],RP,IP,WP) ->
    {Data,_Addr1} = ffe:buffer_read(Addr, U),
    try binary_to_float(Data) of
	Float->
	    ?next([?TRUE,Float|SP],RP,IP,WP)
    catch
	error:_ ->
	    ?next([?FALSE|SP],RP,IP,WP)
    end.

words() ->
    #{
      ?WORD("n>f", 'n>f'),
      ?WORD("f>n", 'f>n'),
      ?WORD("f+", 'f+'),
      ?WORD("f-", 'f-'),
      ?WORD("f*", 'f*'),
      ?WORD("f/", 'f/'),
      ?WORD("f**", 'f**'),
      ?WORD("fabs",fabs),
      ?WORD("ffloor", ffloor),
      ?WORD("fmax", fmax),
      ?WORD("fmin", fmin),
      ?WORD("fnegate", fnegate),
      ?WORD("fround", fround),
      ?WORD("fsqrt", fsqrt),
      ?WORD("f0<", 'f0<'),
      ?WORD("f0=", 'f0='),
      ?WORD("f<", 'f<'),
      ?WORD("has-floating", has_floating),
      ?WORD("has-floating-stack", has_floating_stack),
      ?WORD("floats", floats),
      ?WORD("floats+", floats_plus),
      ?WORD(">float", to_float)
     }.

%% Borrowed from io_lib_format:fwrite_g/1
format(0.0) ->
    "0.0";
format(Float) when is_float(Float) ->
    {Frac, Exp} = mantissa_exponent(Float),
    {Place, Digits} = fwrite_g_1(Float, Exp, Frac),
    R = insert_decimal(Place, [$0 + D || D <- Digits]),
    [$- || true <- [Float < 0.0]] ++ R.

-define(BIG_POW, (1 bsl 52)).
-define(MIN_EXP, (-1074)).

mantissa_exponent(F) ->
    case <<F:64/float>> of
        <<_S:1, 0:11, M:52>> -> % denormalized
            E = log2floor(M),
            {M bsl (53 - E), E - 52 - 1075};
        <<_S:1, BE:11, M:52>> when BE < 2047 ->
            {M + ?BIG_POW, BE - 1075}
    end.

fwrite_g_1(Float, Exp, Frac) ->
    Round = (Frac band 1) =:= 0,
    if 
        Exp >= 0  ->
            BExp = 1 bsl Exp,
            if
                Frac =:= ?BIG_POW ->
                    scale(Frac * BExp * 4, 4, BExp * 2, BExp,
                          Round, Round, Float);
                true ->
                    scale(Frac * BExp * 2, 2, BExp, BExp,
                          Round, Round, Float)
            end;
        Exp < ?MIN_EXP ->
            BExp = 1 bsl (?MIN_EXP - Exp),
            scale(Frac * 2, 1 bsl (1 - Exp), BExp, BExp,
                  Round, Round, Float);
        Exp > ?MIN_EXP, Frac =:= ?BIG_POW ->
            scale(Frac * 4, 1 bsl (2 - Exp), 2, 1,
                  Round, Round, Float);
        true ->
            scale(Frac * 2, 1 bsl (1 - Exp), 1, 1,
                  Round, Round, Float)
    end.

scale(R, S, MPlus, MMinus, LowOk, HighOk, Float) ->
    Est = int_ceil(math:log10(abs(Float)) - 1.0e-10),
    %% Note that the scheme implementation uses a 326 element look-up
    %% table for int_pow(10, N) where we do not.
    if
        Est >= 0 ->
            fixup(R, S * int_pow(10, Est), MPlus, MMinus, Est,
                  LowOk, HighOk);
        true ->
            Scale = int_pow(10, -Est),
            fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est,
                  LowOk, HighOk)
    end.

fixup(R, S, MPlus, MMinus, K, LowOk, HighOk) ->
    TooLow = if 
                 HighOk ->  R + MPlus >= S;
                 true -> R + MPlus > S
             end,
    case TooLow of
        true ->
            {K + 1, generate(R, S, MPlus, MMinus, LowOk, HighOk)};
        false ->
            {K, generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)}
    end.

generate(R0, S, MPlus, MMinus, LowOk, HighOk) ->
    D = R0 div S,
    R = R0 rem S,
    TC1 = if
              LowOk -> R =< MMinus;
              true -> R < MMinus
          end,
    TC2 = if
              HighOk -> R + MPlus >= S;
              true -> R + MPlus > S
          end,
    case {TC1, TC2} of
        {false, false} -> 
            [D | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)];
        {false, true} ->
            [D + 1];
        {true, false} -> 
            [D];
        {true, true} when R * 2 < S ->
            [D];
        {true, true} ->
            [D + 1]
    end.

insert_decimal(0, S) ->
    "0." ++ S;
insert_decimal(Place, S) ->
    L = length(S),
    if
        Place < 0;
        Place >= L ->
            ExpL = integer_to_list(Place - 1),
            ExpDot = if L =:= 1 -> 2; true -> 1 end,
            ExpCost = length(ExpL) + 1 + ExpDot,
            if 
                Place < 0 ->
                    if 
                        2 - Place =< ExpCost ->
                            "0." ++ lists:duplicate(-Place, $0) ++ S;
                        true ->
                            insert_exp(ExpL, S)
                    end;
                true ->
                    if
                        Place - L + 2 =< ExpCost ->
                            S ++ lists:duplicate(Place - L, $0) ++ ".0";
                        true ->
                            insert_exp(ExpL, S)
                    end
            end;
        true ->
            {S0, S1} = lists:split(Place, S),
            S0 ++ "." ++ S1
    end.

insert_exp(ExpL, [C]) ->
    [C] ++ ".0e" ++ ExpL;
insert_exp(ExpL, [C | S]) ->
    [C] ++ "." ++ S ++ "e" ++ ExpL.

int_ceil(X) when is_float(X) ->
    T = trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

int_pow(X, 0) when is_integer(X) ->
    1;
int_pow(X, N) when is_integer(X), is_integer(N), N > 0 ->
    int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

log2floor(Int) when is_integer(Int), Int > 0 ->
    log2floor(Int, 0).

log2floor(0, N) ->
    N;
log2floor(Int, N) ->
    log2floor(Int bsr 1, 1 + N).
