-module(fmath).
-compile(export_all).
-ffe_file("fmath.fs").
-ffe_export([sqr, sum]).

'_ffe_sqr'() ->
  {0, <<"sqr">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_dupe'/0,
  fun 'ffe':'_ffe_star'/0,
  fun 'ffe':'semis'/0}.
'_ffe_sum'() ->
  {0, <<"sum">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_zero'/0,
  fun 'ffe':'pdo'/0,
  fun 'ffe':'_ffe_plus'/0,
  fun 'ffe':'ploop'/0,
  -2,
  fun 'ffe':'semis'/0}.
