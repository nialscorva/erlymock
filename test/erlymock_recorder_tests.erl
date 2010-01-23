%% @author jason
%% @copyright jason Jan 21, 2010  PROPRIETARY-- NOT FOR DISTRIBUTION
%% @doc TODO: Add description to erlymock_call_recorder_tests
%% @end
%% --------------------------------------------------------------------
-module(erlymock_recorder_tests).
-include_lib("eunit/include/eunit.hrl").

% External exports
-export([]).
% test cases for the module with setup and teardown
%% erlymock_call_recorder_tests_test_() ->
%%   { foreach,
%%      fun() -> setup() end,  % setup
%%      fun(SetupRetVal) -> teardown(SetupRetVal) end, % teardown
%%      [
%%         ?_assert(hey_you =:= write_some_tests)
%%      ]
%%    }.
%% 
%% setup() -> ok.
%% 
%% teardown(_SetupRetval) -> ok.



simple_strict_test() ->
  Handle=erlymock_recorder:new(),
  H2=erlymock_recorder:strict(Handle,{mod,func},[arg1],[{return,ok}]),
  
  ?assertMatch({ok,_} , erlymock_recorder:invoke(H2,{mod,func},[arg1])).

series_strict_test()->
  H1=erlymock_recorder:new(),
  H2=erlymock_recorder:strict(H1,{mod,func},[arg1],[{return,ok}]),
  H3=erlymock_recorder:strict(H2,{mod,func2},[arg1],[{return,ok}]),
  H4=erlymock_recorder:strict(H3,{mod,func3},[arg1],[{return,ok}]),

  {ok,H5} = erlymock_recorder:invoke(H4,{mod,func},[arg1]),
  {ok,H6} = erlymock_recorder:invoke(H5,{mod,func2},[arg1]),
  {ok,_H7} = erlymock_recorder:invoke(H6,{mod,func3},[arg1]).

bad_series_strict_test()->
  H1=erlymock_recorder:new(),
  H2=erlymock_recorder:strict(H1,{mod,func},[arg1],[{return,ok}]),
  H3=erlymock_recorder:strict(H2,{mod,func2},[arg1],[{return,ok}]),
  H4=erlymock_recorder:strict(H3,{mod,func3},[arg1],[{return,ok}]),

  {ok,H5} = erlymock_recorder:invoke(H4,{mod,func},[arg1]),
  ?assertThrow({erlymock,no_match,_}, erlymock_recorder:invoke(H5,{mod,func3},[arg1])),
  {ok,_H7} = erlymock_recorder:invoke(H5,{mod,func2},[arg1]).


simple_o_o_test() ->
  Handle=erlymock_recorder:new(),
  H2=erlymock_recorder:strict(Handle,{mod,func},[arg1],[{return,ok}]),
  
  ?assertMatch({ok,_H3} , erlymock_recorder:invoke(H2,{mod,func},[arg1])).

series_o_o_test()->
  H1=erlymock_recorder:new(),
  H2=erlymock_recorder:stub(H1,{mod,func},[arg1],[{return,ok},{max_invocations,1}]),
  H3=erlymock_recorder:stub(H2,{mod,func2},[arg1],[{return,ok},{max_invocations,1}]),
  H4=erlymock_recorder:stub(H3,{mod,func3},[arg1],[{return,ok},{max_invocations,1}]),

  {ok,H5} = erlymock_recorder:invoke(H4,{mod,func3},[arg1]),
  {ok,H6} = erlymock_recorder:invoke(H5,{mod,func},[arg1]),
  {ok,_H7} = erlymock_recorder:invoke(H6,{mod,func2},[arg1]).

bad_series_o_o_test()->
  H1=erlymock_recorder:new(),
  H2=erlymock_recorder:stub(H1,{mod,func},[arg1],[{return,ok},{max_invocations,1}]),
  H3=erlymock_recorder:stub(H2,{mod,func2},[arg1],[{return,ok},{max_invocations,1}]),
  H4=erlymock_recorder:stub(H3,{mod,func3},[arg1],[{return,ok},{max_invocations,1}]),

  {ok,H5} = erlymock_recorder:invoke(H4,{mod,func3},[arg1]),
  ?assertThrow({erlymock,too_many_invocations,_},erlymock_recorder:invoke(H5,{mod,func3},[arg1])),
  {ok,_H7} = erlymock_recorder:invoke(H5,{mod,func2},[arg1]).

simple_stub_test() ->
  Handle=erlymock_recorder:new(),
  H2=erlymock_recorder:strict(Handle,{mod,func},[arg1],[{return,ok}]),
  
  ?assertMatch({ok,_H3} , erlymock_recorder:invoke(H2,{mod,func},[arg1])).
