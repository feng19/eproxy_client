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
            {ok, Socks4Port} = application:get_env(eproxy_client, local_socks4_port),
            epc_sup:start_child(socks4,Socks4Port),
            {ok, Socks5Port} = application:get_env(eproxy_client, local_socks5_port),
            epc_sup:start_child(socks5,Socks5Port),
            %{ok, HttpPort} = application:get_env(eproxy_client, local_http_port),
            %epc_sup:start_child(http,HttpPort),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%%%===================================================================
