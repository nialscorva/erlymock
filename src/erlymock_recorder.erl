%% @author jason
%% @copyright jason Jan 21, 2010  PROPRIETARY-- NOT FOR DISTRIBUTION
%% @doc TODO: Add description to erlymock_call_recorder
%% @end
%% --------------------------------------------------------------------
-module(erlymock_recorder).

-export([new/0,strict/3,stub/3,strict/4,stub/4,invoke/3,map/2,foldl/3,validate_constraints/1]).

-record(expectations,{strict=[], stub=[]}).
-define(DEFAULT_ARGS,[]).

new() ->
  #expectations{}.

strict(#expectations{strict=S}=Handle,Function, Args) when is_record(Handle,expectations),is_list(Args) ->
  Handle#expectations{strict = S ++ [{Function,Args,?DEFAULT_ARGS}]}.

strict(#expectations{strict=S}=Handle,Function, Args, Options)  when is_record(Handle,expectations),is_list(Args), is_list(Options)->
  Handle#expectations{strict = S ++ [{Function,Args,Options}]}.

stub(Handle,Function, Args)  when is_record(Handle,expectations),is_list(Args)->
  stub(Handle,Function, Args, ?DEFAULT_ARGS).

stub(#expectations{stub=S}=Handle,Function, Args, Options)  when is_record(Handle,expectations),is_list(Args), is_list(Options) ->
  Handle#expectations{stub = [{Function,Args,Options} | S]}.

invoke(Handle,Function, Args)  when is_record(Handle,expectations),is_list(Args)->
  case invoke_strict(Handle,Function,Args) of
    not_found -> 
      case invoke_stub(Handle,Function,Args) of
        not_found -> throw({erlymock,unexpected_invocation,[{mfa,Function,Args},{state,Handle}]});
        Any -> Any
      end;
    Any -> Any
  end.

invoke_strict(#expectations{strict=[{Func,Args,Options} | T]}=Handle,CalledFunction,CalledArgs) when Func =:= CalledFunction, is_list(CalledArgs) ->
  RV=case match_args(Args,CalledArgs) of
    true -> Ret=return(Options,CalledArgs),
            {Ret,Handle#expectations{strict=T}};
    _ -> not_found
  end,
  RV;
invoke_strict(_,_,_) ->
  not_found.

invoke_stub(#expectations{stub=Stubs}=Handle,Function,Args) when is_list(Args)->
  Result=lists:foldl(fun(R,{false,Records,_}) -> 
                          {Matched,R2,RV}=match_func(R,Function,Args),
                          {Matched,Records ++ [R2],RV};
                      (R,{true,Records,RV}) -> 
                          {true,Records ++ [R],RV}
                     end, 
                     {false,[], undefined}, Stubs),
  case Result of
    {true, Records,Ret} -> {Ret,Handle#expectations{stub=Records}};
    _ -> not_found
  end.

match_func({Func,Pattern,Options}=Rec, CalledFunc, CalledArgs) when Func =:= CalledFunc, is_list(CalledArgs) ->
  Invocations=proplists:get_value(invocations,Options,0) +1,
  MaxInvocations = proplists:get_value(max_invocations,Options, Invocations+1),
  case match_args(Pattern,CalledArgs) of
    true when MaxInvocations >= Invocations -> 
      {true, {Func,Pattern, [{invocations,Invocations} | proplists:delete(invocations,Options)]}, return(Options,CalledArgs)};
    true -> 
      throw({erlymock,too_many_invocations,[{mfa,CalledFunc,CalledArgs},{invocations,Invocations,MaxInvocations}]});
    _ -> {false,Rec,undefined}
  end;
match_func(Rec, _CalledFunc, _CalledArgs) ->
  {false,Rec,undefined}.


match_args(Pattern,CalledArgs) ->
  lists:all(fun({P,A}) ->  (P == '_') or (P == A) end, lists:zip(Pattern,CalledArgs)).

-define(UNDEF_FLAG,{erlymock,undefined_value}).

return(Options,Args) ->
  case proplists:get_value(exit,Options,?UNDEF_FLAG) of
    ?UNDEF_FLAG -> return_error(Options,Args); 
    Any -> exit(Any)
  end.

return_error(Options,Args) ->
  case proplists:get_value(error,Options,?UNDEF_FLAG) of
    ?UNDEF_FLAG -> return_throw(Options,Args); 
    Any -> erlang:error(Any,Args)
  end.

return_throw(Options,Args) ->
  case proplists:get_value(throw,Options,?UNDEF_FLAG) of
    ?UNDEF_FLAG -> return_function(Options,Args);
    Any -> throw(Any)
  end.

return_function(Options,Args) ->
  case proplists:get_value(function,Options,?UNDEF_FLAG) of
    ?UNDEF_FLAG -> proplists:get_value(return,Options,ok);
    Any -> apply(Any,Args)
  end.

map(Func, #expectations{stub=Stub,strict=Strict}) when is_function(Func) ->
  lists:map(Func,Stub) ++ lists:map(Func,Strict).
  
foldl(Func, Acc0,#expectations{stub=Stub,strict=Strict}) when is_function(Func) ->
  Acc1=lists:foldl(Func,Acc0,Stub),
  lists:foldl(Func,Acc1,Strict).

validate_constraints(#expectations{stub=Stub,strict=Strict}) ->
  StrictProblems = lists:map(fun (S) -> {not_called, S} end, Strict),
  StubProblems = lists:foldl(fun validate_stub/2,[],Stub),
  StrictProblems ++ StubProblems.

validate_stub({_Func,_Pattern, Options}=P,Acc) ->
  Invocations = proplists:get_value(invocations,Options,0),
  Min = proplists:get_value(min_invocations,Options,0),
  Max = proplists:get_value(max_invocations,Options,unlimited),
  case Max of
    _X when Invocations < Min -> [ {too_few_calls,P} | Acc];
    X when Invocations > X -> [ {too_many_calls,P} | Acc ];
    _ -> Acc
  end.