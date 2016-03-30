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
            Socks5Port = application:get_env(eproxy_client, local_socks5_port, 7070),
            epc_sup:start_child(socks5,Socks5Port),
            Socks4Port = application:get_env(eproxy_client, local_socks4_port, 7171),
            epc_sup:start_child(socks4,Socks4Port),
            HttpPort = application:get_env(eproxy_client, local_http_port, 7272),
            epc_sup:start_child(http,HttpPort),
            io:format("eproxy client start succeed~n"),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%%%===================================================================
