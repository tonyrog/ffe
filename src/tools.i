%% -*- erlang -*-
%%   FFE tools words
%%
-ifndef(__TOOLS__).
-define(__TOOLS__, true).

tools_words() ->
    #{
      ?WORD(".s",         emit_stack),
      ?WORD("words",      emit_words)
     }.

emit_stack() ->
    { 0, <<".s">>, fun ffe:emit_stack/4 }.
emit_stack(SP, RP, IP, WP) ->
    lists:foreach(fun(Value) ->
			  emit_value(Value),
			  emit_char($\s)
		  end, lists:reverse(SP)),
    next(SP, RP, IP, WP).

?XT("words", emit_words).
emit_words(SP, RP, IP, WP) ->
    Words = collect_words([current() | forth()], []),
    format_word_list(altout(), Words),
    next(SP, RP, IP, WP).

collect_words([Dict|Ds], Acc) ->
    Acc1  = collect_dict(Dict, Acc),
    collect_words(Ds, Acc1);
collect_words([], Acc) ->
    Acc.

collect_dict(Dict, Acc0) ->
    maps:fold(
      fun(Name, _Xt, Acc) ->
	      [Name | Acc]
      end, Acc0, Dict).

-endif.
