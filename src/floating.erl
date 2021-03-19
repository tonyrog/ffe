%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    FFE floating support
%%% @end
%%% Created : 19 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(floating).

-export([words/0]).
-export(['i>f'/0, 'i>f'/4]).
-export(['f>i'/0, 'f>i'/4]).
-export(['f+'/0, 'f+'/4]).
-export(['f-'/0, 'f-'/4]).
-export(['f*'/0, 'f*'/4]).
-export(['f/'/0, 'f/'/4]).
-export(['f**'/0, 'f**'/4]).
-export(['fabs'/0, 'fabs'/4]).
-export(['ffloor'/0, 'ffloor'/4]).
-export(['fmax'/0, 'fmax'/4]).
-export(['fmin'/0, 'fmin'/4]).
-export(['fnegate'/0, 'fnegate'/4]).
-export(['fround'/0, 'fround'/4]).
-export(['fsqrt'/0, 'fsqrt'/4]).

'i>f'() ->
    { 0, <<"i>f">>, fun floating:'i>f'/4 }.
'i>f'([I|SP],RP,IP,WP) ->
    ffe:next([float(I)|SP],RP,IP,WP).


'f>i'() ->
    { 0, <<"f>i">>, fun floating:'f>i'/4 }.
'f>i'([F|SP],RP,IP,WP) ->
    ffe:next([trunc(F)|SP],RP,IP,WP).

'f+'() ->
    { 0, <<"f+">>, fun floating:'f+'/4 }.
'f+'([A,B|SP],RP,IP,WP) ->
    ffe:next([B+A|SP],RP,IP,WP).

'f-'() ->
    { 0, <<"f-">>, fun floating:'f-'/4 }.
'f-'([A,B|SP],RP,IP,WP) ->
    ffe:next([B-A|SP],RP,IP,WP).

'f*'() ->
    { 0, <<"f*">>, fun floating:'f*'/4 }.
'f*'([A,B|SP],RP,IP,WP) ->
    ffe:next([B*A|SP],RP,IP,WP).

'f/'() ->
    { 0, <<"f/">>, fun floating:'f/'/4 }.
'f/'([A,B|SP],RP,IP,WP) ->
    ffe:next([B/A|SP],RP,IP,WP).

'f**'() ->
    { 0, <<"f**">>, fun floating:'f**'/4 }.
'f**'([A,B|SP],RP,IP,WP) ->
    ffe:next([math:pow(B,A)|SP],RP,IP,WP).

'fabs'() ->
    { 0, <<"fabs">>, fun floating:'fabs'/4 }.
'fabs'([A|SP],RP,IP,WP) ->
    ffe:next([abs(A)|SP],RP,IP,WP).

'ffloor'() ->
    { 0, <<"ffloor">>, fun floating:'ffloor'/4 }.
'ffloor'([A|SP],RP,IP,WP) ->
    ffe:next([math:floor(A)|SP],RP,IP,WP).

'fmin'() ->
    { 0, <<"fmin">>, fun floating:'fmin'/4 }.
'fmin'([A,B|SP],RP,IP,WP) ->
    ffe:next([min(B,A)|SP],RP,IP,WP).

'fmax'() ->
    { 0, <<"fmax">>, fun floating:'fmax'/4 }.
'fmax'([A,B|SP],RP,IP,WP) ->
    ffe:next([max(B,A)|SP],RP,IP,WP).

'fnegate'() ->
    { 0, <<"fnegate">>, fun floating:'fnegate'/4 }.
'fnegate'([A|SP],RP,IP,WP) ->
    ffe:next([-A|SP],RP,IP,WP).

'fround'() ->
    { 0, <<"fround">>, fun floating:'fround'/4 }.
'fround'([A|SP],RP,IP,WP) ->
    ffe:next([round(A)|SP],RP,IP,WP).

'fsqrt'() ->
    { 0, <<"fsqrt">>, fun floating:'fsqrt'/4 }.
'fsqrt'([A|SP],RP,IP,WP) ->
    ffe:next([math:sqrt(A)|SP],RP,IP,WP).


words() ->
    #{
      <<"i>f">> => fun floating:'i>f'/0,
      <<"f>i">> => fun floating:'f>i'/0,
      <<"f+">> => fun floating:'f+'/0,
      <<"f-">> => fun floating:'f-'/0,
      <<"f*">> => fun floating:'f*'/0,
      <<"f/">> => fun floating:'f/'/0,
      <<"f**">> => fun floating:'f**'/0,
      <<"fabs">> => fun floating:'fabs'/0,
      <<"ffloor">> => fun floating:'ffloor'/0,
      <<"fmax">> => fun floating:'fmax'/0,
      <<"fmin">> => fun floating:'fmin'/0,
      <<"fnegate">> => fun floating:'fnegate'/0,
      <<"fround">> => fun floating:'fround'/0,
      <<"fsqrt">> => fun floating:'fsqrt'/0
     }.

      
      
