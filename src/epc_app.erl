-module(epc_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

%%%===================================================================

start(_StartType, _StartArgs) ->
    case epc_sup:start_link() of
        {ok, Pid} ->
            epc_sup:start_child(socks5),
            epc_sup:start_child(socks4),
            epc_sup:start_child(http),
            io:format("eproxy client start succeed~n"),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%%%===================================================================
