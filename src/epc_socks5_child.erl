-module(epc_socks5_child).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {key, lsock, socket, remote_pid}).

-include("socks_type.hrl").
-include("debug.hrl").
-define(TIMEOUT, 1000 * 60 * 10).

%%%===================================================================
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%%===================================================================
init([LSock]) ->
    ?DEBUG("init LSock:~p",[LSock]),
    {ok, Key} = application:get_env(eproxy_client, key),
    {ok, #state{key=Key,lsock=LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(timeout, #state{key=Key,lsock=LSock,socket=undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    epc_socks5_sup:start_child(),

    case start_process(Socket, Key) of
        {ok, RemotePid} ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{socket=Socket, remote_pid=RemotePid}, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, Error, State}
    end;

%% send by OPT timeout
handle_info(timeout, #state{socket = Socket} = State) when is_port(Socket) ->
    ?DEBUG("timeout stop"),
    {stop, timeout, State};

%% recv from client, and send to remote
handle_info({tcp, Socket, Request}, #state{key=Key, socket=Socket, remote_pid=RemotePid} = State) ->
    Binary = epc_crypto:encrypt(Key, Request),
    epc_ws_handler:send(RemotePid, Binary),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};

%% recv from remote, and send back to client
handle_info({websocket_msg,Response}, #state{key=Key, socket=Client} = State) ->
    {ok, RealData} = epc_crypto:decrypt(Key, Response),
    case gen_tcp:send(Client, RealData) of
        ok ->
            ok = inet:setopts(Client, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, Error, State}
    end;

handle_info({tcp_closed, _}, State) ->
    ?DEBUG("tcp_closed stop"),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    ?DEBUG("tcp_error stop:~p",[Reason]),
    {stop, Reason, State};

handle_info(close, State) ->
    ?DEBUG("close"),
    {stop, normal, State}.

terminate(Reason, #state{socket=Socket, remote_pid=RemotePid}) ->
    ?DEBUG("terminate:~p",[Reason]),
    case is_pid(RemotePid) of
        true ->
            epc_ws_handler:close(RemotePid);
        false ->
            ok
    end,
    case is_port(Socket) of
        true -> 
            gen_tcp:close(Socket);
        false -> 
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

start_process(Socket, Key) ->
    {ok, RemoteAddr} = application:get_env(eproxy_client, remote_addr),
    case epc_ws_handler:start_link(RemoteAddr,self()) of
        {ok, RemotePid} ->
            {ok, Target} = find_target(Socket),
            EncryptedTarget = epc_crypto:encrypt(Key, Target),
            epc_ws_handler:send(RemotePid, EncryptedTarget),
            ok = gen_tcp:send(Socket, <<5,0,0,1,0,0,0,0,0,0>>),
            {ok, RemotePid};
        {error, Error} ->
            {error, Error}
    end.


%% -spec(find_target(Socket :: port()) -> {ok, <<_:32, _:_*8>>}).
find_target(Socket) ->
    {ok, <<5:8, _Nmethods:8,_Bin/binary>>} = gen_tcp:recv(Socket, 0),
    %todo auth

    gen_tcp:send(Socket, <<5:8, 0:8>>),
    {ok, <<5:8, 1:8, _Rsv:8, AType:8,Rest/binary>>} = gen_tcp:recv(Socket, 0),

    case AType of
        ?IPV4 ->
            <<Address:32, Port:16, _/binary>> = Rest,
            ?DEBUG(<<Address:32>>),
            {ok, <<?IPV4, Port:16, Address:32>>};
        ?IPV6 ->
            <<Address:128, Port:16, _/binary>> = Rest,
            ?DEBUG(<<Address:32>>),
            {ok, <<?IPV6, Port:16, Address:128>>};
        ?DOMAIN ->
            <<DomainLen:8, DomainBin:DomainLen/binary, Port:16,_/binary>> = Rest,
            ?DEBUG(DomainBin),
            {ok, <<?DOMAIN, Port:16, DomainLen:8, DomainBin/binary>>}
    end.

