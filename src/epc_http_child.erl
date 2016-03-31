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

handle_call(Request, _From, State) ->
    ?DEBUG("call Request:~p",[Request]),
    {reply, ok, State}.

handle_cast(Info, State) ->
    ?DEBUG("cast info:~p",[Info]),
    {noreply, State}.

handle_info({tcp, Socket, Request}, #state{remote_pid=undefined} = State) ->
%%    ?DEBUG("tcp Request:~p",[Request]),
    case parse_request(Request) of
        {http, Target, NormalizedReqeust} ->
            case start_process(2) of
                {ok, RemotePid} ->
                    epc_ws_handler:send(RemotePid,Target),
                    epc_ws_handler:send(RemotePid,NormalizedReqeust),
                    ok = inet:setopts(Socket, [{active, once}]),
                    {noreply, State#state{remote_pid=RemotePid}, ?TIMEOUT};
                {error, Error} ->
                    ?DEBUG("error stop:~p",[Error]),
                    {stop, normal, State}
            end;
        {connect,Target} ->
            case start_process(2) of
                {ok, RemotePid} ->
                    epc_ws_handler:send(RemotePid,Target),
                    gen_tcp:send(Socket, <<"HTTP/1.1 200 Connection Established\r\n\r\n">>),
                    ok = inet:setopts(Socket, [{active, once}]),
                    {noreply, State#state{remote_pid=RemotePid}, ?TIMEOUT};
                {error, Error} ->
                    ?DEBUG("error stop:~p",[Error]),
                    {stop, normal, State}
            end;
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, normal, State}
    end;
handle_info({tcp, Socket, Request}, #state{remote_pid=RemotePid} = State) ->
%%    ?DEBUG("tcp Request:~p",[Request]),
    epc_ws_handler:send(RemotePid,Request),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State, ?TIMEOUT};
%% recv from remote, and send back to client
handle_info({websocket_msg,Response}, State) ->
    gen_tcp:send(State#state.socket, Response),
    {noreply, State, ?TIMEOUT};
handle_info(timeout, #state{lsock = LSock, socket = undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    epc_http_sup:start_child(),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket}, ?TIMEOUT};
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

start_process(N) ->
    case epc_ws_handler:start_link() of
        {ok, RemotePid} ->
            {ok, RemotePid};
        {error, timeout} when N=/=0 ->
            start_process(N-1);
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

