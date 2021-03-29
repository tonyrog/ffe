-ifndef(__STRAP__).
-define(__STRAP__, true).
'+!'() ->
  {0, <<"+!">>,
  fun ffe:docol/4,
  fun ffe:dup/0,
  fun ffe:fetch/0,
  fun ffe:rot/0,
  fun ffe:plus/0,
  fun ffe:swap/0,
  fun ffe:store/0,
  fun ffe:semis/0}.
'++'() ->
  {0, <<"++">>,
  fun ffe:docol/4,
  fun ffe:one/0,
  fun ffe:swap/0,
  fun ffe:'+!'/0,
  fun ffe:semis/0}.
'--'() ->
  {0, <<"--">>,
  fun ffe:docol/4,
  fun ffe:minus_one/0,
  fun ffe:swap/0,
  fun ffe:'+!'/0,
  fun ffe:semis/0}.
'<'() ->
  {0, <<"<">>,
  fun ffe:docol/4,
  fun ffe:minus/0,
  fun ffe:zero_less/0,
  fun ffe:semis/0}.
'='() ->
  {0, <<"=">>,
  fun ffe:docol/4,
  fun ffe:minus/0,
  fun ffe:zero_equals/0,
  fun ffe:semis/0}.
'>'() ->
  {0, <<">">>,
  fun ffe:docol/4,
  fun ffe:swap/0,
  fun ffe:'<'/0,
  fun ffe:semis/0}.
'NULL'() ->
  {0, <<"NULL">>,
  fun ffe:docol/4,
  fun ffe:rfrom/0,
  fun ffe:drop/0,
  fun ffe:semis/0}.
decimal() ->
  {0, <<"decimal">>,
  fun ffe:docol/4,
  fun ffe:lit/0,
  10,
  fun ffe:base/0,
  fun ffe:store/0,
  fun ffe:semis/0}.
hex() ->
  {0, <<"hex">>,
  fun ffe:docol/4,
  fun ffe:lit/0,
  16,
  fun ffe:base/0,
  fun ffe:store/0,
  fun ffe:semis/0}.
nip() ->
  {0, <<"nip">>,
  fun ffe:docol/4,
  fun ffe:swap/0,
  fun ffe:drop/0,
  fun ffe:semis/0}.
octal() ->
  {0, <<"octal">>,
  fun ffe:docol/4,
  fun ffe:lit/0,
  8,
  fun ffe:base/0,
  fun ffe:store/0,
  fun ffe:semis/0}.
tuck() ->
  {0, <<"tuck">>,
  fun ffe:docol/4,
  fun ffe:swap/0,
  fun ffe:over/0,
  fun ffe:semis/0}.
strap_words() ->
  #{
      <<"+!">> => fun ffe:'+!'/0,
      <<"++">> => fun ffe:'++'/0,
      <<"--">> => fun ffe:'--'/0,
      <<"<">> => fun ffe:'<'/0,
      <<"=">> => fun ffe:'='/0,
      <<">">> => fun ffe:'>'/0,
      <<"NULL">> => fun ffe:'NULL'/0,
      <<"bin">> => fun ffe:bin/0,
      <<"decimal">> => fun ffe:decimal/0,
      <<"hex">> => fun ffe:hex/0,
      <<"nip">> => fun ffe:nip/0,
      <<"octal">> => fun ffe:octal/0,
      <<"tuck">> => fun ffe:tuck/0,
      <<>> => false
  }.
-endif.
