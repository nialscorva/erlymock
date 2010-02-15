%% @author Jason Wagner
%% @copyright Jason Wagner Jan 2010 Distributed under MIT License
%% @doc Erlymock_tcp mocks a tcp connection.
%% @end
%% --------------------------------------------------------------------
-module(erlymock_tcp).

-behaviour(gen_server).

% External exports
-export([open/0,open/1,o_o/2,o_o/3,strict/2,strict/3,stub/2,stub/3,wipe_tcp/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(DEFAULT_OPTS, [{socket_options,[binary, {packet, 2}, {active, false}]}]).
-define(SERVER,?MODULE).
-define(TAG,tcp_socket).

-record(state, {mock_side, client_side, state=init}).


% --------------------------------------------------------------------
%% @spec open() -> {ok, Socket} 
%% @doc Opens a socket using default options that will be mocked on the other end.  
%% Note that only one socket can be opened in any test.
%% @end 
% --------------------------------------------------------------------
open() ->
  open(?DEFAULT_OPTS).

% --------------------------------------------------------------------
%% @spec open( [Option ]) -> {ok, Socket::client_socket()}
%%  Option = {port, Port::integer()} 
%%         | {socket_options, GenTCPOptions}
%% @doc Opens a socket that will be mocked on the other end.  Note that only
%% one socket can be opened in any test.  socket options are defined in the
%% gen_tcp page and will be applied to both sides of the connection.  The only caveat
%% is that both sides will be set {active,false}.  The mock side will be active once
%% erlymock:replay() is called.  The port is optional, if one is not provided then
%% the system will assign one.
%% @end 
% --------------------------------------------------------------------
open(Options) when is_list(Options) ->
  catch(gen_server:call(?SERVER,{halt})),  % make sure a stale instance isn't hanging around
  wipe_tcp(), % any leftover closes, etc
  {ok,Pid}=gen_server:start_link({local,?SERVER},?MODULE,[Options],[]),
  erlymock:internal_register(Pid),
  Socket=gen_server:call(Pid,{get_socket,self()}),
  {ok, Socket}.  


% --------------------------------------------------------------------
%% @spec strict(Socket::client_socket(),Verifier::verifier()) -> ok
%% @doc Requires the socket to receive a value that matches the Verifier.  
%% No response is made on the socket.
%% @end
% --------------------------------------------------------------------
strict(Socket,Verifier) when is_binary(Verifier) or is_function(Verifier) or is_list(Verifier)->
  strict(Socket,Verifier,[]).

% --------------------------------------------------------------------
%% @spec strict(Socket::client_socket(),Verifier::verifier(),[verification_option()]) -> ok
%% @doc Requires the socket to receive a value that matches the Verifier binary
%% or causes the Verifier function to return true.
%% @end
% --------------------------------------------------------------------
strict(Socket,Verifier, Options) when is_list(Verifier),is_list(Options)->
  strict(Socket,list_to_binary(Verifier),Options);
strict(Socket,Verifier, Options) when is_function(Verifier),is_list(Options)->
  erlymock:internal_strict({?TAG,Socket}, Verifier, make_options(Options));
strict(Socket,Verifier, Options) when is_binary(Verifier),is_list(Options)->
  erlymock:internal_strict({?TAG,Socket}, [Verifier], make_options(Options)).

% --------------------------------------------------------------------
%% @spec o_o(Socket::client_socket(),Verifier::verifier()) -> ok
%% @doc Requires the socket to receive a value that matches the Verifier binary
%% or causes the Verifier function to return true once and only once in an unspecified
%% order.  Alias for stub(..., [{max_invocations,1},{min_invocations,1}])
%% @end
% --------------------------------------------------------------------
o_o(Socket,Verifier) when (is_binary(Verifier) or is_function(Verifier) or is_list(Verifier))->
  o_o(Socket,Verifier,[]).

% --------------------------------------------------------------------
%% @spec o_o(Socket::client_socket(),Verifier::verifier(),[verification_option()]) -> ok
%% @doc Requires the socket to receive a value that matches the Verifier binary
%% or causes the Verifier function to return true once and only once in an unspecified
%% order.  Alias for stub(..., [{max_invocations,1},{min_invocations,1} | Options])
%% @end
% --------------------------------------------------------------------
o_o(Socket,Verifier,Options) when (is_binary(Verifier) or is_function(Verifier) or is_list(Verifier)),is_list(Options)->
  stub(Socket,Verifier,[{max_invocations,1},{min_invocations,1} | Options]).

% --------------------------------------------------------------------
%% @spec stub(Socket::client_socket(),Verifier::verifier()) -> ok
%% @doc Requires the socket to receive a value that matches the Verifier binary
%% or causes the Verifier function to return true.
%% @end
% --------------------------------------------------------------------
stub(Socket,Verifier) when (is_binary(Verifier) or is_function(Verifier) or is_list(Verifier))->
  stub(Socket,Verifier,[]).

% --------------------------------------------------------------------
%% @spec stub(Socket::client_socket(),Verifier::verifier(),[verification_option()]) -> ok
%% @doc Requires the socket to receive a value that matches the Verifier binary
%% or causes the Verifier function to return true.
%% @end
% --------------------------------------------------------------------
stub(Socket,Verifier, Options) when is_list(Verifier), is_list(Options)->
  stub(Socket,list_to_binary(Verifier),Options);
stub(Socket,Verifier, Options) when is_function(Verifier), is_list(Options)->
  erlymock:internal_stub({?TAG,Socket}, Verifier, make_options(Options));
stub(Socket,Verifier, Options) when is_binary(Verifier), is_list(Options)->
  erlymock:internal_stub({?TAG,Socket}, [Verifier], make_options(Options)).


% --------------------------------------------------------------------
%% @spec init([]) ->
%%          {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% @private
%% @doc Initiates the server
%% @end 
% --------------------------------------------------------------------
init([Options]) ->
  process_flag(trap_exit,true),
  Self = self(),
  ListenPort=proplists:get_value(port,Options,0),
  spawn(fun() ->
             {ok, ListenSocket} = gen_tcp:listen(ListenPort,server_options(Options)),
             {ok,P}=inet:port(ListenSocket),
             Self ! {ready, P},
             {ok, Socket} = gen_tcp:accept(ListenSocket),
             gen_tcp:controlling_process(Socket, Self),
             Self ! {takeover,Socket}
        end),
  {ok, ClientSocket} =receive
    {ready,Port} -> gen_tcp:connect('localhost', Port, client_options(Options))
    after 5000 -> timeout_waiting_for_server_ready
  end,
  receive
    {takeover,ServerSocket} ->
      {ok, #state{client_side=ClientSocket, mock_side=ServerSocket}}
    after 5000 -> {stop, timeout_on_connect}
  end.

server_options(Options) ->
  [{active,false} | proplists:delete(active,proplists:get_value(socket_options,Options,[]))].

client_options(Options) ->
  [{active,false} | proplists:delete(active,proplists:get_value(socket_options,Options,[]))].


% --------------------------------------------------------------------
%% @spec handle_call(Request::term(), From::pid(), State::state()) ->
%%          {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   |
%%          {stop, Reason, State}
%% @private
%% @doc Handling call messages
%% @end
% --------------------------------------------------------------------
handle_call({get_socket,NewOwner}, _From, #state{client_side=Socket,state=init}=State) ->
  gen_tcp:controlling_process(Socket, NewOwner),
  {reply,Socket,State};
handle_call({erlymock_state,replay}, _From, #state{mock_side=ServerSocket}=State) ->
  inet:setopts(ServerSocket, [{active,once}]),
  {reply,true,State#state{state=replay}};
handle_call({erlymock_state,verify}, _From, State) ->
  {reply,true,State#state{state=done}};
handle_call({halt},_From,State) ->
  {stop, normal, ok, State}.

% --------------------------------------------------------------------
%% @spec handle_cast(Msg::term(), State::state()) ->
%%          {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%% @private
%% @doc Handling cast messages
%% @end
% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
%% @spec handle_info(Info::term(), State::state()) ->
%%          {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%% @private
%% @doc Handling all non call/cast messages
%% @end
% --------------------------------------------------------------------
handle_info({tcp,_,Data},#state{client_side=Socket,mock_side=MockSocket}=State) ->
  X=erlymock:internal_invocation_event({?TAG,Socket}, [Data]),
  io:format("Got data ~p~nInvocation returned ~p~n", [Data,X]),
  
  case X of
    {ok,{reply,Reply}} when is_binary(Reply) -> gen_tcp:send(MockSocket, Reply);
    {ok,{close}} -> gen_tcp:close(MockSocket);
    {ok,_Any} -> io:format("Invocation was ignored ~p~n",[_Any]);
    Any -> erlymock:internal_error({erlymock_tcp_error, {data,Data}, Any})
  end,
  inet:setopts(MockSocket, [{active,once}]),
  {noreply, State};
handle_info({tcp_closed,_}, State) ->
  {stop, normal, State};
handle_info({'EXIT',_,_}, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  io:format("Received ~p~n",[_Info]),
  {noreply, State}.

% --------------------------------------------------------------------
%% @spec terminate(Reason::term(),State::state()) -> any()
%% @private
%% @doc Shutdown the server
%% @end
% --------------------------------------------------------------------
terminate(_Reason, #state{mock_side=MockSocket}) ->
  gen_tcp:close(MockSocket),
  ok.

% --------------------------------------------------------------------
%% @spec code_change(OldVsn,State,Extra) -> {ok, NewState}
%% @private
%% @doc Convert process state when code is changed
%% @end
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


make_options(Options) ->
  lists:foldl(fun({reply, Val},Acc) when is_binary(Val) -> [{return,{reply,Val}} | Acc];
                 ({reply, Val},Acc) when is_list(Val) -> [{return,{reply,list_to_binary(Val)}} | Acc];
             (close,Acc) -> [{return, {close}} | Acc];
             ({close,true},Acc) -> [{return, {close}} | Acc ];
             ({max_invocations,D} = A,Acc) when is_number(D) -> [A | Acc];
             ({min_invocations,D} = A,Acc) when is_number(D) -> [A | Acc];
             (Any,_) -> throw({erlymock_invalid_option, Any})
            end, [], Options).


wipe_tcp() ->
  receive
    {tcp,_,_} = A -> io:format("wiped ~p~n",[A]),wipe_tcp();
    {tcp_closed,_} =A  -> io:format("wiped ~p~n",[A]),wipe_tcp()
    after 0 -> ok
  end.

%% @type verification_option() = close | {close,true} 
%%                             | {max_invocations, Count::integer()} 
%%                             | {min_invocations, Count::integer()} 
%%                             | {reply, Response::binary()}.

%% @type verifier() = binary() | function().  Function must be of form fun(Arg::binary()) and return true if the Arg is acceptible.
%% Any non-true value or failed match will be interpretted as false.

%% @type client_socket().  A handle to a gen_tcp socket, usable for all such functions.