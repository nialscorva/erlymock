%% @author jason
%% @copyright jason Jan 24, 2010  PROPRIETARY-- NOT FOR DISTRIBUTION
%% @doc TODO: Add description to erlymock_tcp_tests
%% @end
%% --------------------------------------------------------------------
-module(erlymock_tcp_tests).
-include_lib("eunit/include/eunit.hrl").


basic_strict_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(2000),
  erlymock_tcp:strict(Socket,<<"test packet">>),
  erlymock:replay(),
  gen_tcp:send(Socket,<<"test packet">>),
  erlymock:verify().

basic_series_strict_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(2000),
  erlymock_tcp:strict(Socket,<<"test packet">>),
  erlymock_tcp:strict(Socket,<<"test packet2">>),
  erlymock_tcp:strict(Socket,<<"test packet3">>),
  erlymock:replay(),
  gen_tcp:send(Socket,<<"test packet">>),
  gen_tcp:send(Socket,<<"test packet2">>),
  gen_tcp:send(Socket,<<"test packet3">>),
  erlymock:verify().

fail_out_of_order_strict_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(2000),
  erlymock_tcp:strict(Socket,<<"test packet">>),
  erlymock_tcp:strict(Socket,<<"test packet2">>),
  erlymock_tcp:strict(Socket,<<"test packet3">>),
  erlymock:replay(),
  gen_tcp:send(Socket,<<"test packet2">>),
  gen_tcp:send(Socket,<<"test packet">>),
  gen_tcp:send(Socket,<<"test packet3">>),
  timer:sleep(500),
  ?assertThrow(_,erlymock:verify()).

reply_to_packet_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(2000),
  erlymock_tcp:strict(Socket,<<"test packet">>,[{reply,<<"reply">>}]),
  erlymock:replay(),
  inet:setopts(Socket, [{active,true}]),

  gen_tcp:send(Socket,<<"test packet">>),
  % cannot assert inside of receive without problems, it seems
  RV=receive
    {tcp,_,<<"reply">>} -> true;
    {tcp,_,Data} -> {received_incorrect_data,Data};
    Any -> {received_incorrect_data,Any}
    after 500 -> reply_timeout
  end,
  ?assert(RV).
                             
close_connection_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(2000),
  erlymock_tcp:strict(Socket,<<"test packet">>,[close]),
  erlymock:replay(),
  inet:setopts(Socket, [{active,true}]),
  
  gen_tcp:send(Socket,<<"test packet">>),
  % cannot assert inside of receive without problems, it seems
  RV=receive
    {tcp_closed,_} -> true;
    {tcp,_,Data} -> {should_not_receive_data,Data}
    after 500 -> reply_timeout
  end,
  
  ?assert(RV).
