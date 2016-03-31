-module(eproxy_client).
-export([
    main/1,
    start/0,
    debug/3,
    debug/4
]).

main(_) ->
    {ok,_} = start(),
    receive
        stop ->
            stop
    end.
start() ->
    application:ensure_all_started(?MODULE).

-ifdef(debug).

debug(Module,Line,Msg) ->
    io:format("[~p:~p] ~p~n",[Module,Line,Msg]).
debug(Module,Line,Format,Args) ->
    io:format("[~p:~p] "++Format++"~n",[Module,Line|Args]).

-else.

debug(_Module,_Line,_Msg) ->
    ok.
debug(_Module,_Line,_Format,_Args) ->
    ok.

-endif.
