-module(epc_socks5_child).
-include("epc.hrl").
-behaviour(gen_server).

-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).
-export([start_link/1]).

-record(state, {lsock, socket, remote_pid}).

%%%===================================================================
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%%===================================================================
init([LSock]) ->
%%     ?DEBUG("init LSock:~p",[LSock]),
    process_flag(trap_exit, true),
    {ok, #state{lsock=LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

%% recv from client, and send to remote
handle_info({tcp, Socket, Request}, #state{socket=Socket, remote_pid=RemotePid} = State) ->
    epc_ws_handler:send(RemotePid,Request),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};
%% recv from remote, and send back to client
handle_info({websocket_msg, Response}, #state{socket=Client} = State) ->
    case gen_tcp:send(Client, Response) of
        ok ->
            ok = inet:setopts(Client, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, Error, State}
    end;
handle_info(timeout, #state{lsock=LSock,socket=undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    epc_socks5_sup:start_child(),
    {ok, Target} = find_target(Socket),
    case start_process(Socket, Target, 2) of
        {ok, RemotePid} ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{socket=Socket, remote_pid=RemotePid}, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, Error, State}
    end;
handle_info(timeout, State) ->
%%     ?DEBUG("timeout stop"),
    {stop, normal, State};
handle_info({tcp_closed, _}, State) ->
    ?DEBUG("tcp_closed stop"),
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    ?DEBUG("tcp_error stop:~p",[Reason]),
    {stop, Reason, State};
handle_info({'EXIT',_RemotePid,normal}, State) ->
    {stop, normal, State};
handle_info({'EXIT',_RemotePid,Reason}, State) ->
    ?DEBUG("exit:~p",[Reason]),
    {stop, normal, State}.

terminate(_Reason, #state{socket=Socket}) ->
%%     ?DEBUG("terminate:~p",[Reason]),
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

start_process(Socket, Target, N) ->
    case epc_ws_handler:start_link(self()) of
        {ok, RemotePid} ->
            epc_ws_handler:send(RemotePid,Target),
            ok = gen_tcp:send(Socket, <<5,0,0,1,0,0,0,0,0,0>>),
            {ok, RemotePid};
        {error, timeout} when N=/=0 ->
            start_process(Socket, Target, N-1);
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
%%            ?DEBUG(<<Address:32>>),
            {ok, <<?IPV4, Port:16, Address:32>>};
        ?IPV6 ->
            <<Address:128, Port:16, _/binary>> = Rest,
%%            ?DEBUG(<<Address:32>>),
            {ok, <<?IPV6, Port:16, Address:128>>};
        ?DOMAIN ->
            <<DomainLen:8, DomainBin:DomainLen/binary, Port:16,_/binary>> = Rest,
%%            ?DEBUG(DomainBin),
            {ok, <<?DOMAIN, Port:16, DomainLen:8, DomainBin/binary>>}
    end.

