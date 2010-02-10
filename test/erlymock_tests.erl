%% @author Jason Wagner
%% @copyright Jason Wagner Jan 2010 Distributed under MIT License
%% @doc Unit tests for the erlymock public interface
%% @end
%% --------------------------------------------------------------------
-module(erlymock_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


without_module_override_test() ->
  erlymock:start(),
  erlymock:strict(mod,func1,[1,2],[{return,what}]),
  erlymock:replay(),
  erlymock:invocation_event({mod,func1,2},[1,2]).
  
without_module_override_fail_test() ->
  erlymock:start(),
  erlymock:strict(mod,func1,[1,2],[{return,what}]),
  erlymock:replay(),
  ?assertThrow({erlymock,unexpected_invocation,_},erlymock:invocation_event({mod1,func1,2},[1,2])),
  ?assertThrow({erlymock,unexpected_invocation,_},erlymock:invocation_event({mod,func2,2},[1,2])),
  ?assertThrow({erlymock,unexpected_invocation,_},erlymock:invocation_event({mod,func1,3},[1,2,3])),
  ?assertThrow({erlymock,unexpected_invocation,_},erlymock:invocation_event({mod,func1,2},[1,3])).
  


basic_in_order_test() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, [1,2]),
  erlymock:strict( testmodule1, mockme2, [2,3]),
  erlymock:strict( testmodule2, mockme2, [3,2]),
  erlymock:strict( testmodule2, mockme1, [1,2]),

  erlymock:replay(),

  ?assertMatch(ok,testmodule1:mockme1(1,2)),
  ?assertMatch(ok,testmodule1:mockme2(2,3)),
  ?assertMatch(ok,testmodule2:mockme2(3,2)),
  ?assertMatch(ok,testmodule2:mockme1(1,2)),

  ?assertMatch(ok,erlymock:verify()).

incorrect_in_order_test() ->
  erlymock:start(),
  erlymock:strict( testmodule1, mockme1, [1,2], [{return, ok}]),
  erlymock:strict( testmodule1, mockme2, [2,3], [{return, ok}]),
  erlymock:strict( testmodule2, mockme2, [3,2], [{return, ok}]),
  erlymock:strict( testmodule2, mockme1, [1,2], [{return, ok}]),

  erlymock:replay(),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  ?assertThrow({erlymock,unexpected_invocation,_},testmodule2:mockme1(1,2)).

incomplete_in_order_test() ->
  erlymock:start(),
  erlymock:strict( testmodule1, mockme1, [1,2], [{return, ok}]),
  erlymock:strict( testmodule1, mockme2, [2,3], [{return, ok}]),
  erlymock:strict( testmodule2, mockme2, [3,2], [{return, ok}]),
  erlymock:strict( testmodule2, mockme1, [1,2], [{return, ok}]),

  erlymock:replay(),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  testmodule2:mockme2(3,2),

  ?assertThrow({mock_failure, _},erlymock:verify()).

strict_o_o_mix_test() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, [1,2], [{return, ok}]),
  erlymock:o_o   (testmodule1, mockme2, [2,3], [{return, ok}]),
  erlymock:strict(testmodule2, mockme2, [3,2], [{return, ok}]),
  erlymock:o_o   (testmodule2, mockme1, [1,2], [{return, ok}]),

  erlymock:replay(),

  testmodule1:mockme1(1,2),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),
  testmodule1:mockme2(2,3),

  erlymock:verify().

not_all_o_o_called_mixed_with_strict_test() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, [1,2], [{return, ok}]),
  erlymock:o_o   (testmodule1, mockme2, [2,3], [{return, ok}]),
  erlymock:strict(testmodule2, mockme2, [3,2], [{return, ok}]),
  erlymock:o_o   (testmodule2, mockme1, [1,2], [{return, ok}]),

  erlymock:replay(),

  ?assertMatch(ok,testmodule1:mockme1(1,2)),
  ?assertMatch(ok,testmodule2:mockme2(3,2)),
  ?assertMatch(ok,testmodule2:mockme1(1,2)),

  ?assertThrow({mock_failure, _} ,erlymock:verify()).

stubs_mixed_with_o_o_and_strict_test() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, [1,2], [{return, ok}]),
  erlymock:o_o   (testmodule1, mockme2, [2,3], [{return, ok}]),
  erlymock:strict(testmodule2, mockme2, [3,2], [{return, ok}]),
  erlymock:o_o   (testmodule2, mockme1, [1,2], [{return, ok}]),

  erlymock:stub  (testmodule3, mockme, [666], [{return, ok}]),
  erlymock:stub  (testmodule3, mockme, [777], [{return, ok}]),
  erlymock:stub  (testmodule3, mockme, [888], [{return, ok}]),
  erlymock:stub  (testmodule3, mockme, [999], [{return, ok}]),

  erlymock:replay(),

  ?assertMatch(ok,testmodule1:mockme1(1,2)),
  ?assertMatch(ok,testmodule2:mockme2(3,2)),
  ?assertMatch(ok,testmodule3:mockme(777)),
  ?assertMatch(ok,testmodule2:mockme1(1,2)),
  ?assertMatch(ok,testmodule3:mockme(777)),
  ?assertMatch(ok,testmodule3:mockme(888)),
  ?assertMatch(ok,testmodule1:mockme2(2,3)),
  ?assertMatch(ok,testmodule3:mockme(777)),

  erlymock:verify().

empty_mock_test() ->
  erlymock:start(),
  erlymock:replay(),
  erlymock:verify().

cleanup_on_start_test() ->
  erlymock:start(),
  erlymock:strict( testmodule1, mockme1, [1,2], [{return, ok}]),
  erlymock:start(), % should reset the above statement
  erlymock:replay(),
  erlymock:verify().


strict_not_called_test() ->
  erlymock:start(),
  erlymock:strict(testmodule, mockme, [1,2],[{return, ok}]),
  erlymock:replay(),
  ?assertThrow({mock_failure, _}, erlymock:verify()).

error_return_test() ->
  erlymock:start(),
  erlymock:strict(testmodule2, mockme1, [666], [{error, end_of_times}]),
  erlymock:replay(),
  ?assertError(end_of_times, testmodule2:mockme1(666)).

throw_return_test() ->
  erlymock:start(),
  erlymock:o_o(testmodule2, mockme1, [666], [{throw, end_of_times}]),
  erlymock:replay(),
  ?assertThrow(end_of_times,testmodule2:mockme1(666)).

exit_return_test() ->
  erlymock:start(),
  erlymock:strict(testmodule2, mockme1, [666], [{exit, end_of_times}]),
  erlymock:replay(),
  ?assertExit(end_of_times,testmodule2:mockme1(666)).

error_on_no_replay_test() ->
  erlymock:start(),
  ?assertThrow({invalid_state,_},erlymock:verify()).

exit_on_bad_return_param_test() ->
  erlymock:start(),
  ?assertError(_,erlymock:strict(testmodule1, mockme1, [1,2], {not_an_array, xxx})).

function_retval_test() ->
  erlymock:start(),
  erlymock:o_o(testmodule1, mockme1, [1,2], [{function, fun(X,Y) ->  X + Y end}]),
  erlymock:replay(),
  ?assertMatch(3,testmodule1:mockme1(1,2)),
  erlymock:verify().

error_in_fun_retval_test() ->
  erlymock:start(),
  erlymock:stub(testmodule1, mockme1, [1,2], [{function, fun(_,_) ->  erlang:error(test) end}]),
  erlymock:replay(),
  ?assertError(_,testmodule1:mockme1(1,2)),
  erlymock:verify().

function_arg_checking_strict_test() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, fun(1,2) -> true end),
  erlymock:replay(),

  ?assertMatch(ok,testmodule1:mockme1(1,2)),

  ?assertMatch(ok,erlymock:verify()).
  
function_arg_checking_stub_test() ->
  erlymock:start(),
  erlymock:stub(testmodule1, mockme1, fun(1,2) -> true end),
  erlymock:replay(),

  ?assertMatch(ok,testmodule1:mockme1(1,2)),

  ?assertMatch(ok,erlymock:verify()).
