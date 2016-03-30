-module(epc_http_child).
-behaviour(gen_server).
-include("epc.hrl").

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
    {ok, #state{lsock = LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(close, State) ->
    ?DEBUG("close"),
    {stop, normal, State}.

handle_info({tcp, Socket, Request}, #state{socket=Socket,remote_pid=undefined} = State) ->
%%     ?DEBUG(Request),
    case connect_to_remote(Socket,Request,2) of
        {ok, RemotePid} ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{remote_pid=RemotePid}, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, Error, State}
    end;
handle_info({tcp, Socket, Request}, #state{socket=Socket,remote_pid=RemotePid} = State) ->
%%     ?DEBUG(Request),
    epc_ws_handler:send(RemotePid,Request),
    {noreply, State, ?TIMEOUT};
%% recv from remote, and send back to client
handle_info({websocket_msg,Response}, #state{socket=Client} = State) ->
    case gen_tcp:send(Client, Response) of
        ok ->
            ok = inet:setopts(Client, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, Error, State}
    end;
handle_info(timeout, #state{lsock = LSock, socket = undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    epc_http_sup:start_child(),
    {noreply, State#state{socket = Socket}, ?TIMEOUT};
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

terminate(Reason, #state{socket=Socket}) ->
    ?DEBUG("terminate:~p",[Reason]),
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

connect_to_remote(Socket,Request,N) ->
    case epc_ws_handler:start_link(self()) of
        {ok, RemotePid} ->
            case parse_request(Request) of
                {http, Target, NormalizedReqeust} ->
                    epc_ws_handler:send(RemotePid,Target),
                    epc_ws_handler:send(RemotePid,NormalizedReqeust),
                    {ok,RemotePid};
                {connect,Target} ->
                    epc_ws_handler:send(RemotePid,Target),
                    gen_tcp:send(Socket, <<"HTTP/1.1 200 Connection Established\r\n\r\n">>),
                    {ok,RemotePid};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, timeout} when N=/=0 ->
            connect_to_remote(Socket,Request,N-1);
        {error, Error} ->
            {error, Error}
    end.

parse_request(<<"CONNECT ",Rest/binary>>) ->
    case binary:split(Rest, <<"\r\n">>) of
        [FirstLine, _RestLines] ->
            [Uri, _Version] = binary:split(FirstLine, <<" ">>, [global]),
            case parse_uri_connect(Uri) of
                {error,Reason} ->
                    {error,Reason};
                {Domain,Port} ->
                    {connect, <<?DOMAIN, Port:16, (byte_size(Domain)):8, Domain/binary>>}
            end;
        _ ->
            {error, need_more}
    end;
parse_request(Request) ->
    case binary:split(Request, <<"\r\n">>) of
        [FirstLine, RestLines] ->
            [Method, Uri, Version] = binary:split(FirstLine, <<" ">>, [global]),
            case parse_uri(Uri) of
                {error, Reason} ->
                    {error, Reason};
                {Domain, Port, Path} ->
                    NormalizedRequest = <<Method/binary," /",Path/binary," ",Version/binary,"\r\n",RestLines/binary>>,
                    {http, <<?DOMAIN, Port:16, (byte_size(Domain)):8, Domain/binary>>, NormalizedRequest}
            end;
        _ ->
            {error, need_more}
    end.

parse_uri_connect(Uri) ->
    case binary:split(Uri,<<"/">>,[global]) of
        [Host_Port] ->
            parse_host_port(Host_Port);
        [Host_Port,_Path] ->
            parse_host_port(Host_Port);
        _ ->
            {error,error_uri}
    end.

parse_uri(<<"HTTP://",Rest/binary>>) ->
    parse_uri_host(Rest,<<>>);
parse_uri(<<"http://",Rest/binary>>) ->
    parse_uri_host(Rest,<<>>);
parse_uri(Uri) ->
    parse_uri_host(Uri,<<>>).

parse_uri_host(<<"/",Path/binary>>,Host) ->
    {Host,80,Path};
parse_uri_host(<<":",Rest/binary>>,Host) ->
    parse_uri_port(Rest,Host,<<>>);
parse_uri_host(<<C,Rest/binary>>,SoFar) ->
    parse_uri_host(Rest,<<SoFar/binary,C>>).

parse_uri_port(<<"/",Path/binary>>,Host,Port) ->
    {Host,binary_to_integer(Port),Path};
parse_uri_port(<<C,Rest/binary>>,Host,Port) ->
    parse_uri_port(Rest,Host,<<Port/binary,C>>).

parse_host_port(Host_Port) ->
    case binary:split(Host_Port,<<":">>) of
        [Host,Port] ->
            {Host,binary_to_integer(Port)};
        _ ->
            {error,error_host_port}
    end.

