%% -*- erlang -*-
-ifndef(__STRAP__).
-define(__STRAP__, true).
'_ffe_+!'() ->
  {0, <<"+!">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_dupe'/0,
  fun 'ffe':'_ffe_fetch'/0,
  fun 'ffe':'_ffe_rote'/0,
  fun 'ffe':'_ffe_plus'/0,
  fun 'ffe':'_ffe_swap'/0,
  fun 'ffe':'_ffe_store'/0,
  fun 'ffe':'semis'/0}.
'_ffe_++'() ->
  {0, <<"++">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_one'/0,
  fun 'ffe':'_ffe_swap'/0,
  fun 'ffe':'-semicolon/4-fun-0-'/0,
  fun 'ffe':'semis'/0}.
'_ffe_--'() ->
  {0, <<"--">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_minus_one'/0,
  fun 'ffe':'_ffe_swap'/0,
  fun 'ffe':'-semicolon/4-fun-0-'/0,
  fun 'ffe':'semis'/0}.
'_ffe_<'() ->
  {0, <<"<">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_minus'/0,
  fun 'ffe':'_ffe_zero_less'/0,
  fun 'ffe':'semis'/0}.
'_ffe_='() ->
  {0, <<"=">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_minus'/0,
  fun 'ffe':'_ffe_zero_equals'/0,
  fun 'ffe':'semis'/0}.
'_ffe_>'() ->
  {0, <<">">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_swap'/0,
  fun 'ffe':'-semicolon/4-fun-0-'/0,
  fun 'ffe':'semis'/0}.
'_ffe_NULL'() ->
  {0, <<"NULL">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'_ffe_rfrom'/0,
  fun 'ffe':'_ffe_drop'/0,
  fun 'ffe':'semis'/0}.
'_ffe_bin'() ->
  {0, <<"bin">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'lit'/0,
  2,
  fun 'ffe':'_ffe_base'/0,
  fun 'ffe':'_ffe_store'/0,
  fun 'ffe':'semis'/0}.
'_ffe_decimal'() ->
  {0, <<"decimal">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'lit'/0,
  10,
  fun 'ffe':'_ffe_base'/0,
  fun 'ffe':'_ffe_store'/0,
  fun 'ffe':'semis'/0}.
'_ffe_hex'() ->
  {0, <<"hex">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'lit'/0,
  16,
  fun 'ffe':'_ffe_base'/0,
  fun 'ffe':'_ffe_store'/0,
  fun 'ffe':'semis'/0}.
'_ffe_octal'() ->
  {0, <<"octal">>,
  fun 'ffe':'docol'/4,
  fun 'ffe':'lit'/0,
  8,
  fun 'ffe':'_ffe_base'/0,
  fun 'ffe':'_ffe_store'/0,
  fun 'ffe':'semis'/0}.

strap() ->
    #{ <<"+!">> => fun ffe:'_ffe_+!'/0,
       <<"++">> => fun ffe:'_ffe_++'/0,
       <<"--">> => fun ffe:'_ffe_--'/0,
       <<"<">> => fun ffe:'_ffe_<'/0,
       <<"=">> => fun ffe:'_ffe_='/0,
       <<">">> => fun ffe:'_ffe_>'/0,
       <<"NULL">> => fun ffe:'_ffe_NULL'/0,
       <<"bin">> => fun ffe:'_ffe_bin'/0,
       <<"decimal">> => fun ffe:'_ffe_decimal'/0,
       <<"hex">> => fun ffe:'_ffe_hex'/0,
       <<"octal">> => fun ffe:'_ffe_octal'/0
     }.

-endif.
