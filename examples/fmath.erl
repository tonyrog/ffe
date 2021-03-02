%% -*- erlang -*-
-module(fmath).
-compile(export_all).
sqr() ->
  {0, <<"sqr">>,
  fun ffe:docol/4,
  fun ffe:dup/0,
  fun ffe:star/0,
  fun ffe:semis/0}.
sqrsum() ->
  {0, <<"sqrsum">>,
  fun ffe:docol/4,
  fun ffe:zero/0,
  fun ffe:swap/0,
  fun ffe:zero/0,
  fun ffe:pdo/0,
  fun ffe:swap/0,
  fun ffe:dup/0,
  fun ffe:star/0,
  fun ffe:plus/0,
  fun ffe:ploop/0,
  -4,
  fun ffe:semis/0}.
sqrsum2() ->
  {0, <<"sqrsum2">>,
  fun ffe:docol/4,
  fun ffe:dup/0,
  fun ffe:star/0,
  fun ffe:swap/0,
  fun ffe:dup/0,
  fun ffe:star/0,
  fun ffe:plus/0,
  fun ffe:semis/0}.
words() ->
  #{
      <<"sqr">> => fun fmath:sqr/0,
      <<"sqrsum">> => fun fmath:sqrsum/0,
      <<"sqrsum2">> => fun fmath:sqrsum2/0,
      <<>> => false
  }.
