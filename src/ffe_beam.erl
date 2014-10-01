%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Generate beam code:
%%%    Initial idea create literals with tuple of function pointers
%%% @end
%%% Created :  1 Oct 2014 by Tony Rogvall <tony@rogvall.se>

-module(ffe_beam).

-compile(export_all).

%%
%% test code to check an idea
%%

test() ->
    ffe_beam:patch_literal("../ebin/foo1.beam", 
			   [{<<"sqr">>, {0,<<"sqr">>,fun ffe:docol/4,fun ffe:semis/0}}, 
			    {<<"sum">>, {0,<<"sum">>,fun ffe:docol/4,fun ffe:plus/0,fun ffe:semis/0 }}]).

patch_literal(BeamFile, NewLits) ->
    {ok,_Mod,Chunks} = beam_lib:all_chunks(BeamFile),
    {"LitT",OldLit} = lists:keyfind("LitT", 1, Chunks),
    Old = unpack_lit(OldLit),
    io:format("Old = ~w\n", [Old]),
    New = patch_lit(Old, NewLits),
    io:format("New = ~w\n", [New]),
    NewChunk = pack_lit(New),
    %% rewrite Old with data from NewLit
    Chunks1 = lists:keyreplace("LitT",1,Chunks,{"LitT",NewChunk}),
    {ok,BeamData} = beam_lib:build_module(Chunks1),
    file:write_file(BeamFile, BeamData).

unpack_lit(<<Size:32,Compressed/binary>>) ->
    io:format("compressed size = ~w\n", [Size]),
    <<N:32,Tab/binary>> = zlib:uncompress(Compressed),
    io:format("table entries = ~w\n", [N]),
    unpack_lit(Tab, 0).

unpack_lit(<<Sz:32,Ext:Sz/binary,T/binary>>, Index) ->
    [{Index,binary_to_term(Ext)}|unpack_lit(T, Index+1)];
unpack_lit(<<>>, _) -> [].

%% pack literal list
pack_lit(LitList) ->
    NumLiterals = length(LitList),
    LitTab1 = erlang:iolist_to_binary(pack_lit_list(LitList)),
    LitTab2 = <<NumLiterals:32,LitTab1/binary>>,
    LitTab = zlib:compress(LitTab2),
    <<(byte_size(LitTab2)):32,LitTab/binary>>.


pack_lit_list([{_Index,Term}|LitList]) ->
    Ext = term_to_binary(Term),
    [<<(byte_size(Ext)):32, Ext/binary>> | pack_lit_list(LitList)];
pack_lit_list([]) ->
    [].

patch_lit([{Index,Term}|Old], NewLits) ->
    {Term,Value} = lists:keyfind(Term, 1, NewLits),
    [{Index,Value} | patch_lit(Old, NewLits)];
patch_lit([], _NewLits) ->
    [].

