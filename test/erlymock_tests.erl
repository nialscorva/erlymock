-module(erlymock_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

  
basic_in_order_test() ->
  erlymock:start(),
  erlymock:expect(in_order, testmodule1, mockme1, [1,2], {return, ok}),
  erlymock:expect(in_order, testmodule1, mockme2, [2,3], {return, ok}),
  erlymock:expect(in_order, testmodule2, mockme2, [3,2], {return, ok}),
  erlymock:expect(in_order, testmodule2, mockme1, [1,2], {return, ok}),

  erlymock:replay(),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),

  erlymock:verify().

incorrect_in_order_test() ->
  erlymock:start(),
  erlymock:expect(in_order, testmodule1, mockme1, [1,2], {return, ok}),
  erlymock:expect(in_order, testmodule1, mockme2, [2,3], {return, ok}),
  erlymock:expect(in_order, testmodule2, mockme2, [3,2], {return, ok}),
  erlymock:expect(in_order, testmodule2, mockme1, [1,2], {return, ok}),

  erlymock:replay(),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  ?assertError({unexpected_invocation,_},testmodule2:mockme1(1,2)).

incomplete_in_order_test() ->
  erlymock:start(),
  erlymock:expect(in_order, testmodule1, mockme1, [1,2], {return, ok}),
  erlymock:expect(in_order, testmodule1, mockme2, [2,3], {return, ok}),
  erlymock:expect(in_order, testmodule2, mockme2, [3,2], {return, ok}),
  erlymock:expect(in_order, testmodule2, mockme1, [1,2], {return, ok}),

  erlymock:replay(),

  testmodule1:mockme1(1,2),
  testmodule1:mockme2(2,3),
  testmodule2:mockme2(3,2),

  ?assertThrow({mock_failure, _},erlymock:verify()).

strict_o_o_mix_test() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, [1,2], {return, ok}),
  erlymock:o_o   (testmodule1, mockme2, [2,3], {return, ok}),
  erlymock:strict(testmodule2, mockme2, [3,2], {return, ok}),
  erlymock:o_o   (testmodule2, mockme1, [1,2], {return, ok}),

  erlymock:replay(),

  testmodule1:mockme1(1,2),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),
  testmodule1:mockme2(2,3),

  erlymock:verify().

not_all_o_o_called_mixed_with_strict_test() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, [1,2], {return, ok}),
  erlymock:o_o   (testmodule1, mockme2, [2,3], {return, ok}),
  erlymock:strict(testmodule2, mockme2, [3,2], {return, ok}),
  erlymock:o_o   (testmodule2, mockme1, [1,2], {return, ok}),

  erlymock:replay(),

  testmodule1:mockme1(1,2),
  testmodule2:mockme2(3,2),
  testmodule2:mockme1(1,2),

  ?assertThrow({mock_failure, _} ,erlymock:verify()).

stubs_mixed_with_o_o_and_strict_test() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, [1,2], {return, ok}),
  erlymock:o_o   (testmodule1, mockme2, [2,3], {return, ok}),
  erlymock:strict(testmodule2, mockme2, [3,2], {return, ok}),
  erlymock:o_o   (testmodule2, mockme1, [1,2], {return, ok}),

  erlymock:stub  (testmodule3, mockme, [666], {return, ok}),
  erlymock:stub  (testmodule3, mockme, [777], {return, ok}),
  erlymock:stub  (testmodule3, mockme, [888], {return, ok}),
  erlymock:stub  (testmodule3, mockme, [999], {return, ok}),

  erlymock:replay(),

  ok = testmodule1:mockme1(1,2),
  ok = testmodule2:mockme2(3,2),
  ok = testmodule3:mockme(777),
  ok = testmodule2:mockme1(1,2),
  ok = testmodule3:mockme(777),
  ok = testmodule3:mockme(888),
  ok = testmodule1:mockme2(2,3),
  ok = testmodule3:mockme(777),

  erlymock:verify().

empty_mock_test() ->
  erlymock:start(),
  erlymock:replay(),
  erlymock:verify().

cleanup_on_start_test() ->
  erlymock:start(),
  erlymock:expect(in_order, testmodule1, mockme1, [1,2], {return, ok}),
  erlymock:start(), % should reset the above statement
  erlymock:replay(),
  erlymock:verify().


strict_not_called_test() ->
  erlymock:start(),
  erlymock:strict(testmodule, mockme, [1,2],{return, ok}),
  erlymock:replay(),
  ?assertThrow({mock_failure, _}, erlymock:verify()).


error_return_test() ->
  erlymock:start(),
  erlymock:strict(testmodule2, mockme1, [666], {error, end_of_times}),
  erlymock:replay(),
  ?assertError(end_of_times, testmodule2:mockme1(666)),
  erlymock:cleanup().

throw_return_test() ->
  erlymock:start(),
  erlymock:o_o(testmodule2, mockme1, [666], {throw, end_of_times}),
  erlymock:replay(),
  ?assertThrow(end_of_times,testmodule2:mockme1(666)),
  erlymock:cleanup().

exit_return_test() ->
  erlymock:start(),
  erlymock:strict(testmodule2, mockme1, [666], {exit, end_of_times}),
  erlymock:replay(),
  ?assertExit(end_of_times,testmodule2:mockme1(666)),
  erlymock:cleanup().

test4() ->
  erlymock:start(),
  {error,_} = erlymock:verify().

test4a() ->
  erlymock:start(),
  {'EXIT',_} = (catch erlymock:strict(testmodule1, mockme1, [1,2], {hier_steht_was_falsches, xxx})).

test5() ->
  erlymock:start(),
  erlymock:strict(testmodule1, mockme1, [1,2], {rec_msg, self()}),
  erlymock:replay(),
  TestPid = spawn(testmodule1,mockme1,[1,2]),
  TestPid ! test,
  receive
 test ->
     ok,
     erlymock:verify()
  after 1000 ->
     error
  end.

test6() ->
  erlymock:start(),
  erlymock:o_o(testmodule1, mockme1, [1,2], {function, fun(X,Y) ->
            X + Y
           end}),
  erlymock:replay(),
  R = testmodule1:mockme1(1,2),
  erlymock:verify(),
  3 = R.

test6a() ->
  erlymock:start(),
  erlymock:stub(testmodule1, mockme1, [1,2], {function, fun(_,_) ->
            erlang:error(test)
           end}),
  erlymock:replay(),
  {'EXIT',_} = (catch testmodule1:mockme1(1,2)),
  erlymock:verify().

test7() ->
  erlymock:start(),
  erlymock:expect(in_order, testmodule1, mockme1, 1,
  fun([{qXYZ, D, B, A}]) when A >= B andalso B >= D ->
   true
  end,
  {function, fun({qXYZ, D, B, A}) ->
   [B,D|A]
  end}
        ),
  erlymock:replay(),
  L = testmodule1:mockme1({qXYZ, 1,2,3}),
  erlymock:verify(),
  [2,1|3] = L.

test7a() ->
  erlymock:start(),
  erlymock:expect(in_order, testmodule1, mockme1, 1,
  %% hier fehlen die obligatorischen Klammern
  fun({qXYZ, D, B, A}) when A >= B andalso B >= D ->
   true
  end,
  {function, fun({qXYZ, D, B, A}) ->
   [B,D|A]
  end}
        ),
  erlymock:replay(),
  {'EXIT',_}= (catch testmodule1:mockme1({qXYZ, 1,2,3})).
