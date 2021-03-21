%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    FFE floting ext support
%%% @end
%%% Created : 21 Mar 2021 by Tony Rogvall <tony@rogvall.se>

-module(floating_ext).

-include("ffe.hrl").

-export([words/0]).
?EXPORT('f~').
?EXPORT(facos).
?EXPORT(facosh).
?EXPORT(falog).
?EXPORT(fasin).
?EXPORT(fasinh).
?EXPORT(fatan).
?EXPORT(fatan2).
?EXPORT(fatanh).
?EXPORT(fcos).
?EXPORT(fexp).
?EXPORT(fexpm1).
?EXPORT(fln).
?EXPORT(flnp1).
?EXPORT(flog).
?EXPORT(fsincos).
?EXPORT(fsin).
?EXPORT(fsinh).
?EXPORT(ftan).
?EXPORT(ftanh).

?XT('f~').
'f~'([F1,F2,F3|SP],RP,IP,WP) ->
    if F3 > 0.0 ->
	    ?next([?BOOL(abs(F1-F2) < F3) | SP],RP,IP,WP);
       F3 =:= 0.0 ->
	    ?next([?BOOL(F1 =:= F2) | SP],RP,IP,WP);
       F3 < 0.0 ->
	    ?next([?BOOL(abs(F1-F2) < -F3*(abs(F1)+abs(F2)))|SP],RP,IP,WP)
    end.
?XT(facos).
facos([F|SP],RP,IP,WP) -> ?next([math:acos(F)|SP],RP,IP,WP).
?XT(facosh).
facosh([F|SP],RP,IP,WP) -> ?next([math:acosh(F)|SP],RP,IP,WP).
?XT(falog).
falog([F|SP],RP,IP,WP) -> ?next([math:pow(10,F)|SP],RP,IP,WP).
?XT(fasin).
fasin([F|SP],RP,IP,WP) -> ?next([math:asin(F)|SP],RP,IP,WP).
?XT(fasinh).
fasinh([F|SP],RP,IP,WP) -> ?next([math:asinh(F)|SP],RP,IP,WP).
?XT(fatan).
fatan([F|SP],RP,IP,WP) -> ?next([math:atan(F)|SP],RP,IP,WP).
?XT(fatan2).
fatan2([F1,F2|SP],RP,IP,WP) -> ?next([math:atan2(F2,F1)|SP],RP,IP,WP).
?XT(fatanh).
fatanh([F|SP],RP,IP,WP) -> ?next([math:atanh(F)|SP],RP,IP,WP).
?XT(fcos).
fcos([F|SP],RP,IP,WP) -> ?next([math:cos(F)|SP],RP,IP,WP).
?XT(fexp).
fexp([F|SP],RP,IP,WP) -> ?next([math:exp(F)|SP],RP,IP,WP).
?XT(fexpm1).
fexpm1([F|SP],RP,IP,WP) -> ?next([math:exp(F-1)|SP],RP,IP,WP).
?XT(fln).
fln([F|SP],RP,IP,WP) -> ?next([math:log(F)|SP],RP,IP,WP).
?XT(flnp1).
flnp1([F|SP],RP,IP,WP) -> ?next([math:log(1+F)|SP],RP,IP,WP).
?XT(flog).
flog([F|SP],RP,IP,WP) -> ?next([math:log10(F)|SP],RP,IP,WP).
?XT(fsin).
fsin([F|SP],RP,IP,WP) -> ?next([math:sin(F)|SP],RP,IP,WP).
?XT(fsinh).
fsinh([F|SP],RP,IP,WP) -> ?next([math:sinh(F)|SP],RP,IP,WP).
%% first sin then cos (cos is top of stack)
?XT(fsincos).
fsincos([F|SP],RP,IP,WP) -> ?next([math:cos(F),math:sin(F)|SP],RP,IP,WP).
?XT(ftan).
ftan([F|SP],RP,IP,WP) -> ?next([math:tan(F)|SP],RP,IP,WP).
?XT(ftanh).
ftanh([F|SP],RP,IP,WP) -> ?next([math:tanh(F)|SP],RP,IP,WP).

words() ->
    #{
      ?WORD("f~", 'f~'),
      ?WORD("facos", facos),
      ?WORD("facosh", facosh),
      ?WORD("falog", falog),
      ?WORD("fasin", fasin),
      ?WORD("fasinh", fasinh),
      ?WORD("fatan", fatan),
      ?WORD("fatan2", fatan2),
      ?WORD("fatanh", fatanh),
      ?WORD("fcos", fcos),
      ?WORD("fexp", fexp),
      ?WORD("fexpm1", fexpm1),
      ?WORD("fln", fln),
      ?WORD("flnp1", flnp1),
      ?WORD("flog", flog),
      ?WORD("fsincos", fsincos),
      ?WORD("fsin", fsin),
      ?WORD("fsinh", fsinh),
      ?WORD("ftan", ftan),
      ?WORD("ftanh", ftanh)
     }.
