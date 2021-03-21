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

emit_words() ->
    { 0, <<"words">>, fun ffe:emit_words/4 }.
emit_words(SP, RP, IP, WP) ->
    emit_dicts([current() | forth()]),
    next(SP, RP, IP, WP).

emit_dicts([Dict|Ds]) ->
    emit_dict(Dict),
    emit_dicts(Ds);
emit_dicts([]) ->
    ok.

emit_dict(Dict) ->
    maps:fold(
      fun(Name, _Xt, _Acc) ->
	      emit_string(Name),
	      emit_char($\s)
      end, [], Dict).

-endif.
