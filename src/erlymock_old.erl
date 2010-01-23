-module(erlymock_old).
-export([cleanup/0,start/0, expect/5, expect/6, strict/5, strict/4, o_o/4, stub/4, replay/0, verify/0, verify_after_last_call/0, verify_after_last_call/1, invocation_event/1]).

-author('Sven Heyll'). % originally "mock", but changed to prevent naming conflicts
-author('Jason Wagner').
-version(3).

-include_lib("eunit/include/eunit.hrl").

-define(MOCK_PID,erlymock_process).

%% use this to create a new instance of a mock process that is in programming phase
start() ->
  cleanup(),
  Self=self(),
  F=fun() -> erlang:monitor(process,Self),
             program_mock([],[],[]) 
    end,
  Pid=spawn(F),
  register(?MOCK_PID,Pid),
  Pid.

%% expect has the following options:
%% Orderchecking types: in_order, out_of_order, stub;  
%% Answering: {return, ...}|{error, ...}|{throw, ...}|{exit, ...}|{rec_msg, Pid}|{function, Fun(Args)} -> RetVal}|{function1, Fun(ArgList)}
expect(Type, Module, Function, Arguments, Answer = {AT, _}) when is_list(Arguments), AT==return;AT==error;AT==throw;AT==exit;AT==rec_msg;AT==function;AT==function1 ->
  call(?MOCK_PID, {expect, Type, Module, Function, length(Arguments), {Arguments, Answer}}).

%% this version of expect is suited for useing custom argument matchers
expect(Type, Module, Fun, Arity, MatcherFun, Answer) when is_integer(Arity), is_function(MatcherFun)->
  call(?MOCK_PID, {expect, Type, Module, Fun, Arity, {custom, MatcherFun, Answer}}).

%% this is a short cut for expect(.., in_order, ..)
strict(M,F,Arity, Fun, Answer) when is_integer(Arity)->
  expect(in_order, M, F, Arity, Fun, Answer).

%% this is a short cut for expect(.., in_order, ..)
strict(M,F,Args, Answer) ->
  expect(in_order, M, F, Args, Answer).

%% this is a short cut for expect(.., in_order, ..)
o_o(M,F,Args, Answer) ->
  expect(out_of_order, M, F, Args, Answer).

%% this is a short cut for expect(.., in_order, ..)
stub( M,F,Args, Answer) when is_list(Args)->
  expect(stub, M, F, Args, Answer);

%% this is a short cut for expect(.., in_order, ..)
stub(M,F,Arity, Answer) when is_integer(Arity) ->
  expect(stub, M, F, Arity, fun(_) -> true end, Answer).

%% after the programming phase call this to switch to the replay phase
replay() ->
  call(?MOCK_PID, replay).

%% after the verification phase use this to verify that all expected invocations occured
verify() ->    
  verify_after_last_call(0).

%% after the verification phase use this to verify that all expected invocations occured
verify_after_last_call() ->
  verify_after_last_call(1500).
verify_after_last_call(TimeOut) ->
  catch(await(invocation_list_empty, TimeOut)),
  call(?MOCK_PID, verify),
  await(cleanup_finished).

cleanup() ->
  case(whereis(?MOCK_PID)) of
    P when is_pid(P) ->  call(P, cleanup),
                         await(cleanup_finished);
    _ -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utility functions for dealing with mock code called from a new process.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%% internal signal function that send a message in "signal" protocol format to
%% some "await(...)"
signal(Pid, Atom) ->
  Pid ! {mock_signal, Atom}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this block the current process until signal(SameAtom) from another process is
%% invoked
await(Atom) when is_atom(Atom) ->
  await(Atom, 2000).

await(Atom,To) when is_atom(Atom)->       
  receive
    {mock_signal, Atom} -> ok
    after To ->
      fail({timeout, await, Atom})
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fail(Pid, Reason) ->
  Pid ! {error, Reason}.

fail(Reason) ->
  throw({mock_failure, Reason}).

success(Pid) ->
  Pid ! {response, ok},
  success().

success() ->
  test_passed.    

call(Name, Request) ->
  try 
    Name ! {self(), Request}
  catch
    error:badarg -> ?debugFmt("Bad argument calling ~p with ~p, and pid = ~p~n",[Name,Request,whereis(Name)])
  end,
  receive
    {error, Reason} ->
      throw({mock_failure, Reason});
    {response, Response} ->
      Response
  end.

filter_fun(_, _, _, {Arguments, _}) ->    
  fun(Args) ->
       lists:all(fun({A,B}) ->  (B == '_') or (A == B) end, lists:zip(Args,Arguments))
  end;

filter_fun(_, _, _, {custom, Matcher, _}) ->
  Matcher.

answer_fun({_, Answer}) ->    
  Answer;

answer_fun({custom, _, Answer}) ->
  Answer.

module_header_abstract_form(Mod) ->
  [{attribute,0,module,Mod}, 
   {attribute,0,compile,[export_all]}].

fundef_to_abstract_meta_form(Self, Mod, FunName, Arity) ->
  Line = 1,
  Params = [{var, Line, list_to_atom("A" ++ integer_to_list(I))} || I <- seq(1,Arity)],
  {function, Line, FunName, Arity,
   [{clause, Line, 
     Params, [], 
     [{call, Line, 
       {remote, Line, {atom, Line, ?MODULE}, {atom, Line, invocation_event}}, 
       [{tuple, Line, 
         [{string,Line, Self}, {atom, Line, Mod}, {atom,Line, FunName}, {integer, Line, Arity},
          lists:foldr(
           fun(E,R) ->
                {cons, Line, E, R}
           end, 
           {nil, Line}, 
           Params)]}]}]}]}.

compile_and_load_abstract_form(AbsForm) ->
  CompRes = compile:forms(AbsForm),
  {ok, Mod, Code} = CompRes,
  code:purge(Mod),
  code:delete(Mod),
  {module, _} = load_module(Mod, Code).

extract_module_set(Combined) ->
  sets:from_list(lists:map(fun([{M,_,_}|_]) -> M end, Combined)).

program_mock(InOrder, OutOfOrder, Stub) ->
  receive 
    {From, {expect, Type, Mod, Fun, Arity, Arg}} ->
      FunDef = [{Mod, Fun, Arity} | {filter_fun(Mod,Fun,Arity,Arg), answer_fun(Arg)}],
      From ! {response, ok},
      case Type of 
        in_order ->
          program_mock([FunDef | InOrder], OutOfOrder, Stub);
        out_of_order ->
          program_mock(InOrder, [FunDef | OutOfOrder], Stub);
        stub ->
          program_mock(InOrder, OutOfOrder, [FunDef | Stub])
      end;
    
    {From, replay}  ->
      Self = pid_to_list(self()),
      Combined = InOrder ++ OutOfOrder ++ Stub,
      ModuleSet = extract_module_set(Combined),
      sets:fold( fun (Mod, _) ->
                       FunsOfModSet = sets:from_list(
                                       lists:foldl(
                                        fun([{M,F,A}|_], Acc) -> 
                                             if Mod == M -> [{F,A}|Acc];           
                                                true -> Acc
                                             end
                                        end, [], Combined)),
                       HeaderForm = module_header_abstract_form(Mod),
                       FunctionForms = sets:fold(
                                        fun({F,A},FFAcc) -> 
                                             [fundef_to_abstract_meta_form(Self, Mod, F, A)|FFAcc]
                                        end,
                                        [],
                                        FunsOfModSet),
                       compile_and_load_abstract_form(HeaderForm ++ FunctionForms)
                 end,
                 [],
                 ModuleSet),
      From ! {response, ok},
      %% spawn a cleanup process that will call the uninstall fun
      auto_cleanup(fun() ->
                        uninstall(ModuleSet),
                        signal(From, cleanup_finished)
                   end),
      record_invocations(lists:reverse(InOrder), 
                         OutOfOrder, 
                         Stub,
                         fun() -> 
                              signal(From, invocation_list_empty) 
                         end
                        );
    {From, cleanup} ->
      success(From);
    {'DOWN',_Ref,process,_From, _Reason} -> ok;  % exit gracefully
    {From, What} ->     
      fail(From, {invalid_state, What})
  end.

record_invocations([], [], Stub, EmptyFun) when is_function(EmptyFun) ->
  EmptyFun(),
  record_invocations([], [], Stub, undefined);
record_invocations(InOrder, OutOfOrder, Stub, EF) ->
  %% wait for all incoming invocations, expect every invocation and crash if the invocation was not correct
  receive
    Invocation = {ProcUnderTestPid, Mod, Fun, Arity, Args} ->
      InvMatcher = fun ([{M,F,A}|{Pred,_}]) ->
                         {M,F,A} == {Mod, Fun, Arity} andalso Pred(Args)
                   end,
      try 
        case InOrder of
          [Test| _] ->  InvMatcher(Test);
          [] -> false
        end
      of
        true ->       
          [[_|{_,Function}]| IOR] = InOrder,
          ProcUnderTestPid ! {mock_process_gaurd__, Function},
          record_invocations(IOR, OutOfOrder, Stub, EF);
        
        false -> 
          case lists:splitwith(InvMatcher, OutOfOrder) of
            {[OOODef|Rest1],Rest2} ->
              [_|{_,Function}] = OOODef,        
              ProcUnderTestPid ! {mock_process_gaurd__, Function},
              record_invocations(InOrder, Rest1 ++ Rest2, Stub, EF);
            
            {[], _} ->
              case lists:filter(InvMatcher, Stub) of
                [StubDef|_] ->
                  [_|{_,Function}] = StubDef,        
                  ProcUnderTestPid ! {mock_process_gaurd__, Function},
                  record_invocations(InOrder, OutOfOrder, Stub, EF);
                
                _ ->
                  EF(),
                  Reason = {unexpected_invocation, Invocation},
                  ProcUnderTestPid ! {mock_process_gaurd__, {error, Reason}},
                  fail(Reason)
              end
          end
      catch
        ET:EX -> 
          Reason = {matching_function_is_incorrent, Invocation, {ET, EX}},
          ProcUnderTestPid ! {mock_process_gaurd__, {error, Reason}},
          EF(),
          fail(Reason)
      end; 
    
    {From, verify} ->
      case {InOrder,OutOfOrder} of
        {[],[]} -> 
          success(From);
        MissingRest ->
          fail(From,{expected_invocations_missing, MissingRest})
      end;
    {From, cleanup} ->
      success(From);
    {From, What} ->
      EF(),
      fail(From, {invalid_state, What})
  
  end.

invocation_event({MockPidStr, Mod, Fun, Arity, Args}) ->
  MockPid = list_to_pid(MockPidStr),
  MockPid ! {self(), Mod, Fun, Arity, Args},
  receive
    {mock_process_gaurd__, {return, Answer}} -> 
      Answer;
    {mock_process_gaurd__, {error, E}} ->
      erlang:error(E);
    {mock_process_gaurd__, {throw, E}} ->
      throw(E);
    {mock_process_gaurd__, {exit, R}} ->
      exit(R);
    {mock_process_gaurd__, {function, F}} ->
      R = apply(F,Args),
      R;
    {mock_process_gaurd__, {function1, F}} ->
      R = F(Args),
      R;
    {mock_process_gaurd__, {rec_msg, P}} ->
      receive 
        M ->
          P ! M
      end
  end.

seq(A, E) when A > E -> [];
seq(A, E) -> lists:seq(A,E).

uninstall(ModuleSet) ->
  lists:map(fun(Mod) ->
                 code:purge(Mod),
                 code:delete(Mod)
            end, sets:to_list(ModuleSet)).

auto_cleanup(CleanupFun) ->    
  spawn_link(fun() ->
                  erlang:process_flag(trap_exit, true),
                  receive
                    {'EXIT', _From, _Reason} ->  CleanupFun();
                    _Ather -> 
                      error_logger:info_msg("auto cleanup handler ~p received unexpected message  ~p.~n", [self(), _Ather])
                  end
             end).          
