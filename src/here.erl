%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Handle here, word building
%%% @end
%%% Created :  2 Apr 2021 by Tony Rogvall <tony@rogvall.se>

-module(here).

-on_load(init/0).
-export([new/0, new/1]).
-export([allot/2]).
-export([size/1]).
-export([here/1]).
-export([clear/1]).
-export([store/3]).
-export([fetch/2]).
-export([comma/2]).
-export([copy/1]).
-export([trim/1]).

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(ffe), "here_nif"),
    erlang:load_nif(Nif, 0).

new() -> ?nif_stub().
new(_Size) ->  ?nif_stub().

allot(_H, _N) ->  ?nif_stub().
here(_H) ->   ?nif_stub().
size(_H) ->   ?nif_stub().
clear(_H) -> ?nif_stub().
store(_H, _I, _T) -> ?nif_stub().
fetch(_H, _I) -> ?nif_stub().
comma(_H, _T) -> ?nif_stub().
copy(_H) -> ?nif_stub().
trim(_H) -> ?nif_stub().
