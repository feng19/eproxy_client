-module(epc_child).

-behaviour(gen_server).

-export([
    start_link/1
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("debug.hrl").

-record(state, {hander, port, lsock}).

start_link(socks5) ->
    {ok, Port} = application:get_env(eproxy_client, local_socks5_port),
    {ok, LSock} = gen_tcp:listen(Port, [binary,
        {ip, {0, 0, 0, 0}},
        {reuseaddr, true},
        {active, false},
        {backlog, 256}]),
    gen_server:start_link(?MODULE, [epc_socks5,Port,LSock], []);
start_link(http) ->
    {ok, Port} = application:get_env(eproxy_client, local_http_port),
    {ok, LSock} = gen_tcp:listen(Port, [binary,
        {ip, {0, 0, 0, 0}},
        {reuseaddr, true},
        {active, once},
        {backlog, 256}]),
    gen_server:start_link(?MODULE, [epc_http,Port,LSock], []);
start_link(Scheme) ->
    ?DEBUG({unknow_scheme,Scheme}),
    {false,unknow_scheme}.


%%%===================================================================

init([Hander,Port,LSock]) ->
    ?DEBUG("start listen LSock:~p hander:~p port:~p",[Hander,Port,LSock]),
    {ok, #state{hander=Hander, port=Port,lsock=LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(timeout, #state{hander=Hander,port=Port,lsock=LSock} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    ?DEBUG("accept one,socket:~p",[Socket]),
    {ok, Key} = application:get_env(eproxy_client, key),
    {ok, RemoteAddr} = application:get_env(eproxy_client, remote_addr),
    {ok,Pid} = websocket_client:start_link(RemoteAddr, Hander, [Key,Port,Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    ?DEBUG("start ws client:~p",[Pid]),
    {noreply, State, 0}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================


