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
            epc_sup:start_child(local_socks4_port, epc_socks4_sup),
            epc_sup:start_child(local_socks5_port, epc_socks5_sup),
            epc_sup:start_child(local_http_port, epc_http_sup),
            io:format("eproxy client start succeed~n"),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%%%===================================================================
