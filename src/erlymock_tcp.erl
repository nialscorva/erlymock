%% @author Jason Wagner
%% @copyright Jason Wagner Jan 2010 Distributed under MIT License
%% @doc Erlymock_tcp mocks a tcp connection.
%% @end
%% --------------------------------------------------------------------
-module(erlymock_tcp).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

% External exports
-export([open/1,open/2,o_o/2,o_o/3,strict/2,strict/3,stub/2,stub/3]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(DEFAULT_OPTS, [binary, {packet, 2}, {active, false}]).

-define(SERVER,?MODULE).
-define(TAG,tcp_socket).

-record(state, {mock_side, client_side, state=init}).

% --------------------------------------------------------------------
%% @spec open() -> {ok, Handle, ReadSocket} 
%% @doc Initiates the server
%% @end 
% --------------------------------------------------------------------
open(Port) ->
  open(Port,?DEFAULT_OPTS).

% --------------------------------------------------------------------
%% @spec open() -> {ok, Handle, ReadSocket} 
%% @doc Initiates the server
%% @end 
% --------------------------------------------------------------------
open(Port,SocketOpts) when is_integer(Port),is_list(SocketOpts) ->
  catch(gen_server:call(?SERVER,{reset})),  % make sure a stail instance isn't hanging around
  {ok,Pid}=gen_server:start_link({local,?SERVER},?MODULE,[Port,SocketOpts],[]),
  erlymock:internal_register(Pid),
  Socket=gen_server:call(Pid,{get_socket,self()}),
  {ok, Socket}.  


% --------------------------------------------------------------------
%% @spec strict(Module::atom(), Function::atom(), Args::list(term())) -> ok
%% @doc Adds a function to the set of calls that must be called in strict order.  Uses
%% the default options [{return,ok}].
%% @end
% --------------------------------------------------------------------
strict(Socket,Data) when is_binary(Data) or is_function(Data)->
  strict(Socket,Data,[]).

% --------------------------------------------------------------------
%% @spec strict(Module::atom(), Function::atom(), Args::list(term()), Options::option_list()) -> ok
%% @doc Adds a function to the set of calls that must be called in strict order.
%% @end
% --------------------------------------------------------------------
strict(Socket,Data, Options) when is_function(Data),is_list(Options)->
  erlymock:internal_strict({?TAG,Socket}, Data, make_options(Options));
strict(Socket,Data, Options) when is_binary(Data),is_list(Options)->
  erlymock:internal_strict({?TAG,Socket}, [Data], make_options(Options)).

% --------------------------------------------------------------------
%% @spec o_o(Module::atom(),Function::atom(),Args::list(term())) -> ok
%% @doc Adds an out-of-order call with default options.  Equivalent to
%% stub(Module,Function,Args,[{return,ok},{max_invocations,1}]).
%% @end
% --------------------------------------------------------------------
o_o(Socket,Data) when (is_binary(Data) or is_function(Data))->
  o_o(Socket,Data,[]).

% --------------------------------------------------------------------
%% @spec o_o(Module::atom(),Function::atom(),Args::list(term()),Options::option_list()) -> ok
%% @doc Adds an out-of-order call.  Equivalent to
%% stub(Module,Function,Args,[{max_invocations,1} | Options]).
%% @end
% --------------------------------------------------------------------
o_o(Socket,Data,Options) when (is_binary(Data) or is_function(Data)),is_list(Options)->
  stub(Socket,Data,[{max_invocations,1},{min_invocations,1} | Options]).

% --------------------------------------------------------------------
%% @spec stub(Module::atom(),Function::atom(),Args::list(term())) -> ok
%% @doc Adds a stub call. 
%% @end
% --------------------------------------------------------------------
stub(Socket,Data) when (is_binary(Data) or is_function(Data))->
  stub(Socket,Data,[]).

% --------------------------------------------------------------------
%% @spec stub(Module::atom(),Function::atom(),Args::list(term()),Options::option_list()) -> ok
%% @doc Adds a stub call. 
%% @end
% --------------------------------------------------------------------
stub(Socket,Data, Options) when is_function(Data), is_list(Options)->
  erlymock:internal_stub({?TAG,Socket}, Data, make_options(Options));
stub(Socket,Data, Options) when is_binary(Data), is_list(Options)->
  erlymock:internal_stub({?TAG,Socket}, [Data], make_options(Options)).


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
init([Port,SocketOpts]) ->
%%   process_flag(trap_exit,true),
  Self = self(),
  spawn(fun() ->
             {ok, ListenSocket} = gen_tcp:listen(Port,[{reuseaddr,true} | SocketOpts]),
             Self ! {ready},
             {ok, Socket} = gen_tcp:accept(ListenSocket),
             gen_tcp:controlling_process(Socket, Self),
             Self ! {takeover,Socket}
        end),
  {ok, ClientSocket} =receive
    {ready} -> gen_tcp:connect('localhost', Port, SocketOpts)
    after 5000 -> timeout_waiting_for_server_ready
  end,
  receive
    {takeover,ServerSocket} ->
      {ok, #state{client_side=ClientSocket, mock_side=ServerSocket}}
    after 5000 -> {stop, timeout_on_connect}
  end.

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
  {reply,true,State};
handle_call({erlymock_state,verify}, _From, State) ->
  {reply,true,State};
handle_call({reset},_From,State) ->
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
  error_logger:error_report("Got data", [Data]),
  case erlymock:internal_invocation_event({?TAG,Socket}, [Data]) of
    {ok,{reply,Reply}} when is_binary(Reply) -> gen_tcp:send(MockSocket, Reply);
    {ok,{close}} -> gen_tcp:close(MockSocket);
    {ok,_Any} -> ok;
    Any -> erlymock:internal_error({erlymock_tcp_error, {data,Data}, Any})
  end,
  inet:setopts(MockSocket, [{active,once}]),
  {noreply, State};
handle_info({tcp_closed,_}, State) ->
  {stop, normal, State};
handle_info(_Info, State) ->
  {noreply, State}.

% --------------------------------------------------------------------
%% @spec terminate/2(Reason::term(),State::state()) -> any()
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
             (close,Acc) -> [{return, {close}} | Acc];
             ({close,true},Acc) -> [{return, {close}} | Acc ];
             ({max_invocations,D} = A,Acc) when is_number(D) -> [A | Acc];
             ({min_invocations,D} = A,Acc) when is_number(D) -> [A | Acc];
             (Any,_) -> throw({erlymock_invalid_option, Any})
            end, [], Options).
