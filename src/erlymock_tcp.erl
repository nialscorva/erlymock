
-module(erlymock_tcp).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([open/1,rx/1,rx/2,tx/2,close/1]).

%%
%% API Functions
%%

% --------------------------------------------------------------------
%% @spec open( [Option ]) -> {ok, Socket::client_socket()}
%%  Option = Any valid options for the socket.
%% @doc Opens a socket pair and assigns both to the current process.  
%% Socket options are defined in the gen_tcp page and will be applied 
%% to both sides of the connection.
%% @end 
% --------------------------------------------------------------------
open(Options) when is_list(Options) ->
  Self = self(),
  ListenPort=proplists:get_value(port,Options,0),
  spawn(fun() ->
             {ok, ListenSocket} = gen_tcp:listen(ListenPort,Options),
             {ok,P}=inet:port(ListenSocket),
             Self ! {ready, P},
             {ok, Socket} = gen_tcp:accept(ListenSocket),
             gen_tcp:controlling_process(Socket, Self),
             Self ! {takeover,Socket}
        end),
  {ok, ClientSocket} =receive
    {ready,Port} -> gen_tcp:connect('localhost', Port, Options)
    after 5000 -> timeout_waiting_for_server_ready
  end,
  {ok,ServerSocket}=receive
    {takeover,S} -> {ok,S}
    after 5000 -> timeout_waiting_for_server_socket
  end,
  {ok,ClientSocket,ServerSocket}.

rx(Socket) -> rx(Socket,0).

rx(Socket,Len) when is_port(Socket) ->
  {ok,Bin}=gen_tcp:recv(Socket,Len),
  Bin.

tx(Socket,Bin) ->
  gen_tcp:send(Socket,Bin).

close(Socket) when is_port(Socket) ->
  gen_tcp:close(Socket).


