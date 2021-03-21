%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    FFE floating support
%%% @end
%%% Created : 19 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(floating).

-include("ffe.hrl").

-export([words/0]).
?EXPORT('n>f').  %% no standard 
?EXPORT('f>n').
?EXPORT(fdrop).
?EXPORT(fdup).
?EXPORT(fover).
?EXPORT(frot).
?EXPORT(fswap).
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

?XT(fdrop).
fdrop(SP,RP,IP,WP) -> ?drop(SP,RP,IP,WP, ffe:next).

?XT(fdup).
fdup(SP,RP,IP,WP) -> ?dup(SP,RP,IP,WP, ffe:next).

?XT(fover).
fover(SP,RP,IP,WP) -> ?over(SP,RP,IP,WP, ffe:next).

?XT(frot).
frot(SP,RP,IP,WP) -> ?rot(SP,RP,IP,WP, ffe:next).

?XT(fswap).
fswap(SP,RP,IP,WP) -> ?swap(SP,RP,IP,WP, ffe:next).

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
      ?WORD("fegate", fegate),
      ?WORD("fround", fround),
      ?WORD("fsqrt", fsqrt),
      ?WORD("f0<", 'f0<'),
      ?WORD("f0=", 'f0='),
      ?WORD("f<", 'f<')
     }.
