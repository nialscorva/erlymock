-module(erlymock_old_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

  
basic_in_order_test() ->
  erlymock_old:start(),
  erlymock_old:expect(in_order, testmodule1, mockme1, [1,2], {return, ok}),
  erlymock_old:expect(in_order, testmodule1, mockme2, [2,3], {return, ok}),
  erlymock_old:expect(in_order, testmodule2, mockme2, [3,2], {return, ok}),
  erlymock_old:expect(in_order, testmodule2, mockme1, [1,2], {return, ok}),

  erlymock_old:replay(),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),

  erlymock_old:verify().

incorrect_in_order_test() ->
  erlymock_old:start(),
  erlymock_old:expect(in_order, testmodule1, mockme1, [1,2], {return, ok}),
  erlymock_old:expect(in_order, testmodule1, mockme2, [2,3], {return, ok}),
  erlymock_old:expect(in_order, testmodule2, mockme2, [3,2], {return, ok}),
  erlymock_old:expect(in_order, testmodule2, mockme1, [1,2], {return, ok}),

  erlymock_old:replay(),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  ?assertError({unexpected_invocation,_},testmodule2:mockme1(1,2)).

incomplete_in_order_test() ->
  erlymock_old:start(),
  erlymock_old:expect(in_order, testmodule1, mockme1, [1,2], {return, ok}),
  erlymock_old:expect(in_order, testmodule1, mockme2, [2,3], {return, ok}),
  erlymock_old:expect(in_order, testmodule2, mockme2, [3,2], {return, ok}),
  erlymock_old:expect(in_order, testmodule2, mockme1, [1,2], {return, ok}),

  erlymock_old:replay(),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  testmodule2:mockme2(3,2),

  ?assertThrow({mock_failure, _},erlymock_old:verify()).

strict_o_o_mix_test() ->
  erlymock_old:start(),
  erlymock_old:strict(testmodule1, mockme1, [1,2], {return, ok}),
  erlymock_old:o_o   (testmodule1, mockme2, [2,3], {return, ok}),
  erlymock_old:strict(testmodule2, mockme2, [3,2], {return, ok}),
  erlymock_old:o_o   (testmodule2, mockme1, [1,2], {return, ok}),

  erlymock_old:replay(),

  testmodule1:mockme1(1,2),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),
  testmodule1:mockme2(2,3),

  erlymock_old:verify().

not_all_o_o_called_mixed_with_strict_test() ->
  erlymock_old:start(),
  erlymock_old:strict(testmodule1, mockme1, [1,2], {return, ok}),
  erlymock_old:o_o   (testmodule1, mockme2, [2,3], {return, ok}),
  erlymock_old:strict(testmodule2, mockme2, [3,2], {return, ok}),
  erlymock_old:o_o   (testmodule2, mockme1, [1,2], {return, ok}),

  erlymock_old:replay(),

  testmodule1:mockme1(1,2),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),

  ?assertThrow({mock_failure, _} ,erlymock_old:verify()).

stubs_mixed_with_o_o_and_strict_test() ->
  erlymock_old:start(),
  erlymock_old:strict(testmodule1, mockme1, [1,2], {return, ok}),
  erlymock_old:o_o   (testmodule1, mockme2, [2,3], {return, ok}),
  erlymock_old:strict(testmodule2, mockme2, [3,2], {return, ok}),
  erlymock_old:o_o   (testmodule2, mockme1, [1,2], {return, ok}),

  erlymock_old:stub  (testmodule3, mockme, [666], {return, ok}),
  erlymock_old:stub  (testmodule3, mockme, [777], {return, ok}),
  erlymock_old:stub  (testmodule3, mockme, [888], {return, ok}),
  erlymock_old:stub  (testmodule3, mockme, [999], {return, ok}),

  erlymock_old:replay(),

  ok = testmodule1:mockme1(1,2),
  ok = testmodule2:mockme2(3,2),
  ok = testmodule3:mockme(777),
  ok = testmodule2:mockme1(1,2),
  ok = testmodule3:mockme(777),
  ok = testmodule3:mockme(888),
  ok = testmodule1:mockme2(2,3),
  ok = testmodule3:mockme(777),

  erlymock_old:verify().

empty_mock_test() ->
  erlymock_old:start(),
  erlymock_old:replay(),
  erlymock_old:verify().

cleanup_on_start_test() ->
  erlymock_old:start(),
  erlymock_old:expect(in_order, testmodule1, mockme1, [1,2], {return, ok}),
  erlymock_old:start(), % should reset the above statement
  erlymock_old:replay(),
  erlymock_old:verify().


strict_not_called_test() ->
  erlymock_old:start(),
  erlymock_old:strict(testmodule, mockme, [1,2],{return, ok}),
  erlymock_old:replay(),
  ?assertThrow({mock_failure, _}, erlymock_old:verify()).

error_return_test() ->
  erlymock_old:start(),
  erlymock_old:strict(testmodule2, mockme1, [666], {error, end_of_times}),
  erlymock_old:replay(),
  ?assertError(end_of_times, testmodule2:mockme1(666)),
  erlymock_old:cleanup().

throw_return_test() ->
  erlymock_old:start(),
  erlymock_old:o_o(testmodule2, mockme1, [666], {throw, end_of_times}),
  erlymock_old:replay(),
  ?assertThrow(end_of_times,testmodule2:mockme1(666)),
  erlymock_old:cleanup().

exit_return_test() ->
  erlymock_old:start(),
  erlymock_old:strict(testmodule2, mockme1, [666], {exit, end_of_times}),
  erlymock_old:replay(),
  ?assertExit(end_of_times,testmodule2:mockme1(666)),
  erlymock_old:cleanup().

error_on_no_replay_test() ->
  erlymock_old:start(),
  ?assertThrow({mock_failure,_},erlymock_old:verify()).

exit_on_bad_return_param_test() ->
  erlymock_old:start(),
  ?assertError(_,erlymock_old:strict(testmodule1, mockme1, [1,2], {hier_steht_was_falsches, xxx})).

rec_msg_retval_test() ->
  erlymock_old:start(),
  erlymock_old:strict(testmodule1, mockme1, [1,2], {rec_msg, self()}),
  erlymock_old:replay(),
  TestPid = spawn(testmodule1,mockme1,[1,2]),
  TestPid ! test,
  receive
    test ->
     ok,
     erlymock_old:verify()
  after 1000 ->
     ?assert(timed_out)
  end.

function_retval_test() ->
  erlymock_old:start(),
  erlymock_old:o_o(testmodule1, mockme1, [1,2], {function, fun(X,Y) ->  X + Y end}),
  erlymock_old:replay(),
  ?assert(3 =:= testmodule1:mockme1(1,2)),
  erlymock_old:verify().

error_in_fun_retval_test() ->
  erlymock_old:start(),
  erlymock_old:stub(testmodule1, mockme1, [1,2], {function, fun(_,_) ->  erlang:error(test) end}),
  erlymock_old:replay(),
  ?assertError(_,testmodule1:mockme1(1,2)),
  erlymock_old:verify().

complex_function_retval_test() ->
  erlymock_old:start(),
  erlymock_old:expect(in_order, testmodule1, mockme1, 1,
  fun([{qXYZ, D, B, A}]) when A >= B andalso B >= D ->
   true
  end,
  {function, fun({qXYZ, D, B, A}) ->
   [B,D|A]
  end}
        ),
  erlymock_old:replay(),
  ?assert([2,1|3] =:= testmodule1:mockme1({qXYZ, 1,2,3})),
  erlymock_old:verify().

error_in_complex_fun_retval_test() ->
  erlymock_old:start(),
  erlymock_old:expect(in_order, testmodule1, mockme1, 1,
  fun({qXYZ, D, B, A}) when A >= B andalso B >= D ->
   true
  end,
  {function, fun({qXYZ, D, B, A}) ->
   [B,D|A]
  end}
        ),
  erlymock_old:replay(),
  ?assertError(_,testmodule1:mockme1({qXYZ, 1,2,3})).
