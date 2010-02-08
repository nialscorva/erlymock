%% @author jason
%% @copyright jason Jan 21, 2010  PROPRIETARY-- NOT FOR DISTRIBUTION
%% @doc TODO: Add description to mocksocket
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

-record(state, {mock_side, client_side, recorder, problems=[], state=init}).

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
  {ok,Pid}=gen_server:start_link(?MODULE,[Port,SocketOpts],[]),
  erlymock:internal_register(Pid),
  Socket=gen_server:call(Pid,{get_socket,self()}),
  {ok, Socket}.  


% --------------------------------------------------------------------
%% @spec strict(Module::atom(), Function::atom(), Args::list(term())) -> ok
%% @doc Adds a function to the set of calls that must be called in strict order.  Uses
%% the default options [{return,ok}].
%% @end
% --------------------------------------------------------------------
strict(Socket,Data) when is_binary(Data)->
  strict(Socket,Data,[]).

% --------------------------------------------------------------------
%% @spec strict(Module::atom(), Function::atom(), Args::list(term()), Options::option_list()) -> ok
%% @doc Adds a function to the set of calls that must be called in strict order.
%% @end
% --------------------------------------------------------------------
strict(Socket,Data, Options) when is_binary(Data),is_list(Options)->
  erlymock:internal_strict({socket,Socket}, [Data], make_options(Options)).

% --------------------------------------------------------------------
%% @spec o_o(Module::atom(),Function::atom(),Args::list(term())) -> ok
%% @doc Adds an out-of-order call with default options.  Equivalent to
%% stub(Module,Function,Args,[{return,ok},{max_invocations,1}]).
%% @end
% --------------------------------------------------------------------
o_o(Socket,Data) when is_binary(Data)->
  o_o(Socket,Data,[]).

% --------------------------------------------------------------------
%% @spec o_o(Module::atom(),Function::atom(),Args::list(term()),Options::option_list()) -> ok
%% @doc Adds an out-of-order call.  Equivalent to
%% stub(Module,Function,Args,[{max_invocations,1} | Options]).
%% @end
% --------------------------------------------------------------------
o_o(Socket,Data,Options) when is_binary(Data),is_list(Options)->
  stub(Socket,Data,[{max_invocations,1},{min_invocations,1} | Options]).

% --------------------------------------------------------------------
%% @spec stub(Module::atom(),Function::atom(),Args::list(term())) -> ok
%% @doc Adds a stub call. 
%% @end
% --------------------------------------------------------------------
stub(Socket,Data) when is_binary(Data)->
  stub(Socket,Data,[]).

% --------------------------------------------------------------------
%% @spec stub(Module::atom(),Function::atom(),Args::list(term()),Options::option_list()) -> ok
%% @doc Adds a stub call. 
%% @end
% --------------------------------------------------------------------
stub(Socket,Data, Options) when is_binary(Data), is_list(Options)->
  O2=case proplists:get_value(reply,Options,undefined) of
    undefined -> Options;
    Val -> [{return,{reply,Val}} | Options]
  end,
  erlymock:internal_stub({socket,Socket}, [Data], O2).

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
      {ok, #state{client_side=ClientSocket, mock_side=ServerSocket, recorder=erlymock_recorder:new()}}
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
handle_call({erlymock_state,verify}, _From, #state{problems=P}=State) ->
  {stop,normal,P,State}.

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
handle_info({tcp,_,Data},#state{client_side=Socket,mock_side=MockSocket, problems=P}=State) ->
  error_logger:error_report("Got data", [Data]),
  Status=case erlymock:internal_invocation_event({socket,Socket}, [Data]) of
    {ok,{reply,Reply}} when is_binary(Reply) -> gen_tcp:send(MockSocket, Reply);
    {ok,{reply,Reply}} -> exit({bad_reply,Reply});  
    {ok,{close}} -> gen_tcp:close(MockSocket);
    {ok,_Any} -> ok;
    Any -> {erlymock_tcp_error, {data,Data}, Any}
  end,
  inet:setopts(MockSocket, [{active,once}]),
  case Status of
    ok ->  {noreply, State};
    _ -> {noreply, State#state{problems = [Status | P]}}
  end;
handle_info({tcp_closed,_}, State) ->
  {noreply, State};
handle_info(Info, State) ->
  io:format("erlymock_tcp received ~p",[Info]),
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
             (Any,_) -> throw({erlymock_invalid_option, Any})
            end, [], Options).
