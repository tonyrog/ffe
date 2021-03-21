-module(process).
-compile(export_all).

'_ffe_self'() ->
  {0, <<"self">>, fun ?MODULE:process_self/4}.
process_self(SP,RP,IP,WP) ->
    ffe:next([erlang:self()|SP],RP,IP,WP).

'_ffe_send'() ->
  {0, <<"send">>, fun ?MODULE:process_send/4}.
process_send([Pid,Value|SP],RP,IP,WP) ->
    Pid ! Value,
    ffe:next(SP,RP,IP,WP).

'_ffe_recv'() ->
  {0, <<"recv">>, fun ?MODULE:process_recv/4}.
process_recv(SP,RP,IP,WP) ->
    receive
	Value ->
	    ffe:next([Value|SP],RP,IP,WP)
    end.

'_ffe_spawn'() ->
    {0, <<"spawn">>, fun ?MODULE:process_spawn/4}.
process_spawn([Xt|SP],RP,IP,WP) ->
    Dict = ffe:current(),
    Pid = spawn_link(fun() ->
			     ffe:init(),
			     ffe:current(Dict),
			     ffe:exec(Xt(), [])
		     end),
    ffe:next([Pid|SP],RP,IP,WP).
