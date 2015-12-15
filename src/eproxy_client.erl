-module(eproxy_client).
-export([
    start/0,
    debug/1,
    debug/2
]).

start() ->
    application:ensure_all_started(eproxy_client).

-ifdef(debug).

debug(Msg) ->
    io:format("[~p:~p] ~p~n",[?MODULE,?LINE,Msg]).
debug(Format,Args) ->
    io:format("[~p:~p] "++Format++"~n",[?MODULE,?LINE|Args]).

-else.

debug(_Msg) ->
    ok.
debug(_Format,_Args) ->
    ok.

-endif.
