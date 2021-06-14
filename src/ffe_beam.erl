%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Generate beam code:
%%%    Initial idea create literals with tuple of function pointers
%%% @end
%%% Created :  1 Oct 2014 by Tony Rogvall <tony@rogvall.se>

-module(ffe_beam).

-compile(export_all).

test() ->
    compile(fmath,
	    [{0, <<"sqr">>, fun ffe:docol/4,  fun ffe:dup/0,  fun ffe:star/0,
	      fun ffe:semis/0},
	     {0, <<"sqrsum">>, fun ffe:docol/4, fun ffe:zero/0, fun ffe:swap/0,
	      fun ffe:zero/0,  fun ffe:pdo/0,   fun ffe:swap/0, fun ffe:dup/0,
	      fun ffe:star/0,  fun ffe:plus/0,  fun ffe:ploop/0,  -4,
	      fun ffe:semis/0},
	     {0, <<"sqrsum2">>, fun ffe:docol/4, fun ffe:dup/0, fun ffe:star/0,
	      fun ffe:swap/0, fun ffe:dup/0, fun ffe:star/0, fun ffe:plus/0,
	      fun ffe:semis/0}]).

%%
%% Compile ffe into beam
%%
compile(Module, Defs) ->
    init_labels(),
    Code = {Module,
	    [{words,0} |
	     [{binary_to_atom(element(2,Def)),0} || Def <- Defs]],
	    [],
	    lists:flatten(
	      [compile_words(Module, Defs),
	       make_words_function(Module,Defs),
	       make_module_info(Module)]),
	    get_num_labels()},
    beam_asm:module(Code, [], [], []).

compile_words(Module, [Def|Defs]) ->
    Name = element(2, Def),
    CodeLabel = new_label(),
    InfoLabel = new_label(),
    Func = binary_to_atom(Name),
    [ 
      {function, Func, 0, CodeLabel,
       [
	{label, InfoLabel},
	{line, []},  %% fixme from ffe source?
	{func_info, {atom, Module}, {atom, Func}, 0},
	{label, CodeLabel},
	{move, {literal, Def}, {x,0}},
	return]} | compile_words(Module, Defs)];
compile_words(_Module, []) ->
    [].

make_words_function(Module, Defs) ->
    CodeLabel = new_label(),
    InfoLabel = new_label(),    
    WordMap =
	maps:from_list([{element(2,Def), 
			 erlang:make_fun(Module,
					 binary_to_atom(element(2,Def)),
					 0)} || Def <- Defs]),
    {function, words, 0, CodeLabel,
     [
      {label, InfoLabel},
      {line, []},  %% fixme from ffe source?
      {func_info, {atom, Module}, {atom, words}, 0},
      {label, CodeLabel},
      {move, {literal, WordMap}, {x,0}},
      return
     ]}.

make_module_info(Module) ->
    CodeLabel0 = new_label(),
    InfoLabel0 = new_label(),
    CodeLabel1 = new_label(),
    InfoLabel1 = new_label(),
    [
     {function, module_info, 0, CodeLabel0, 
      [
       {label,InfoLabel0},
       {func_info, {atom,Module}, {atom,module_info}, 0},
       {label,CodeLabel0},
       {move, {atom, Module}, {x,0}},
       {line, []},
       {call_ext_only,1,{extfunc,erlang,get_module_info,1}}
      ]},
     {function, module_info, 1, CodeLabel1,
      [
       {label,InfoLabel1},
       {func_info, {atom,Module}, {atom,module_info}, 1},
       {label,CodeLabel1},
       {move, {x,0}, {x,1}},
       {move, {atom, Module}, {x,0}},
       {line, []},
       {call_ext_only,2,{extfunc,erlang,get_module_info,2}}
      ]}
     ].


init_labels() ->
    put(next_label, 1).

%% get number of label (including label 0 which is not used ...? )
get_num_labels() ->
    get(next_label).
    
new_label() ->	    
    L = case get(next_label) of
	    undefined -> 1;
	    Li -> Li
	end,
    put(next_label, L+1),
    L.

