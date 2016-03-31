-module(epc_socks4_child).
-include("epc.hrl").
-behaviour(gen_server).

-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).
-export([start_link/1]).

-record(state, {lsock, socket, remote_pid, status=0}).

%%%===================================================================
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%%===================================================================
init([LSock]) ->
%%     ?DEBUG("init LSock:~p",[LSock]),
    process_flag(trap_exit, true),
    {ok, #state{lsock=LSock}, 0}.

handle_call(Request, _From, State) ->
    ?DEBUG("call Request:~p",[Request]),
    {reply, ok, State}.

handle_cast(Info, State) ->
    ?DEBUG("cast info:~p",[Info]),
    {noreply, State}.

%% recv from client, and send to remote
handle_info({tcp, Socket, Request}, #state{remote_pid=RemotePid, status = 2} = State) ->
%%    ?DEBUG("tcp Request:~p",[Request]),
    epc_ws_handler:send(RemotePid,Request),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};
handle_info({tcp, Socket, Request}, #state{status = 1} = State) ->
%%    ?DEBUG("tcp Request:~p",[Request]),
    case Request of
        <<4:8, 1:8, Port:16, Address:32, Rest/binary>> ->
            Target = find_target(Port, Address, Rest),
            case start_process(Socket, Target, 2) of
                {ok, RemotePid} ->
                    ok = inet:setopts(Socket, [{active, once}]),
                    {noreply, State#state{remote_pid=RemotePid, status = 2}, ?TIMEOUT};
                {error, Error} ->
                    ?DEBUG("error stop:~p",[Error]),
                    {stop, normal, State}
            end;
        Data ->
            ?DEBUG("error data:~p",[Data]),
            {stop, normal, State}
    end;
%% recv from remote, and send back to client
handle_info({websocket_msg,Response}, State) ->
    gen_tcp:send(State#state.socket, Response),
    {noreply, State, ?TIMEOUT};
handle_info(timeout, #state{lsock=LSock,socket=undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    epc_socks4_sup:start_child(),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket, status = 1}, ?TIMEOUT};
handle_info(timeout, State) ->
    ?DEBUG("timeout stop State:~p",[State]),
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
%%    ?DEBUG("terminate:~p",[Reason]),
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
    case epc_ws_handler:start_link() of
        {ok, RemotePid} ->
            epc_ws_handler:send(RemotePid,Target),
            gen_tcp:send(Socket, <<0:8, 16#5a:8, 16#FFFF:16, 16#FFFFFFFF:32>>),
            {ok, RemotePid};
        {error, timeout} when N=/=0 ->
            start_process(Socket, Target, N-1);
        {error, Error} ->
            {error, Error}
    end.

find_target(Port, Address, Rest) when Address =< 16#FF ->
    [_UserId, DomainBin, _] = binary:split(Rest, <<0>>, [global]),
%%    ?DEBUG(DomainBin),
    <<?DOMAIN, Port:16, (byte_size(DomainBin)):8, DomainBin/binary>>;
find_target(Port, Address, _Rest) ->
%%    ?DEBUG(<<Address:32>>),
    <<?IPV4, Port:16, Address:32>>.