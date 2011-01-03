%% @author Jason Wagner
%% @copyright Jason Wagner Jan 2010 Distributed under MIT License
%% @doc Unit tests for erlymock_tcp
%% @end
%% --------------------------------------------------------------------
-module(erlymock_tcp_tests).
-include_lib("eunit/include/eunit.hrl").


basic_strict_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:strict(Socket,<<"test packet">>),
  erlymock:replay(),
  gen_tcp:send(Socket,<<"test packet">>),
  erlymock:verify().

basic_series_strict_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:strict(Socket,<<"test packet">>),
  erlymock_tcp:strict(Socket,<<"test packet2">>),
  erlymock_tcp:strict(Socket,<<"test packet3">>),
  erlymock:replay(),
  gen_tcp:send(Socket,<<"test packet">>),
  gen_tcp:send(Socket,<<"test packet2">>),
  gen_tcp:send(Socket,<<"test packet3">>),
  erlymock:verify().

unexpected_send_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:strict(Socket,<<"test packet">>),
  erlymock_tcp:strict(Socket,<<"test packet2">>),
  erlymock:replay(),
  gen_tcp:send(Socket,<<"test packet">>),
  gen_tcp:send(Socket,<<"spanish inquisition">>),
  gen_tcp:send(Socket,<<"test packet2">>),
  ?assertThrow(_,erlymock:verify()).

fail_out_of_order_strict_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:strict(Socket,<<"test packet">>),
  erlymock_tcp:strict(Socket,<<"test packet2">>),
  erlymock_tcp:strict(Socket,<<"test packet3">>),
  erlymock:replay(),
  gen_tcp:send(Socket,<<"test packet2">>),
  gen_tcp:send(Socket,<<"test packet">>),
  gen_tcp:send(Socket,<<"test packet3">>),
  ?assertThrow(_,erlymock:verify()).

reply_to_packet_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
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
  ?assert(RV),
  erlymock:verify().

shutdown_on_verify_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:strict(Socket,<<"test packet">>),
  erlymock:replay(),
  gen_tcp:send(Socket,<<"test packet">>),
  erlymock:verify(),
  ?assertMatch(undefined,whereis(erlymock)),
  ?assertMatch(undefined,whereis(erlymock_tcp)).


threeway_conversation_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:strict(Socket,<<"test packet">>,[{reply,<<"reply">>}]),
  erlymock_tcp:strict(Socket,<<"test packet2">>),
  erlymock:replay(),
  inet:setopts(Socket, [{active,true}]),
 
  gen_tcp:send(Socket,<<"test packet">>),
  % cannot assert inside of receive without problems, it seems
  RV=receive
    {tcp,_,<<"reply">>} -> gen_tcp:send(Socket,<<"test packet2">>), true;
    {tcp,_,Data} -> {received_incorrect_data,Data};
    Any -> {received_incorrect_data,Any}
    after 500 -> reply_timeout
  end,
  ?assert(RV),
  erlymock:verify().


close_connection_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
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

function_arg_checking_strict_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:strict(Socket, fun(<<"test packet">>) -> true end),
  erlymock:replay(),

  gen_tcp:send(Socket,<<"test packet">>),

  ?assertMatch(ok,erlymock:verify()).

function_arg_checking_stub_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:o_o(Socket, fun(<<"test packet">>) -> true end),
  erlymock:replay(),

  gen_tcp:send(Socket,<<"test packet">>),
  ?assertMatch(ok,erlymock:verify()).

function_arg_checking_fail_test() ->
  erlymock:start(),
  {ok,Socket}=erlymock_tcp:open(),
  erlymock_tcp:o_o(Socket, fun(<<"not matched">>) -> true end),
  erlymock:replay(),

  gen_tcp:send(Socket,<<"test packet">>),
  ?assertThrow(_,erlymock:verify()).
