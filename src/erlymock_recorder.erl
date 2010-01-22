%% @author jason
%% @copyright jason Jan 21, 2010  PROPRIETARY-- NOT FOR DISTRIBUTION
%% @doc TODO: Add description to erlymock_call_recorder
%% @end
%% --------------------------------------------------------------------
-module(erlymock_recorder).

-export([new/0,strict/4, strict/5,o_o/4,o_o/5,stub/4,stub/5,invoke/3]).

-record(expectations,{strict=[], stub=[]}).
-define(DEFAULT_ARGS,[]).

new() ->
  #expectations{}.


strict(Handle,Function, Args, Return) ->
  strict(Handle,Function, Args, Return, ?DEFAULT_ARGS).

strict(#expectations{strict=S}=Handle,Function, Args, Return, Options) ->
  Handle#expectations{strict = S ++ [{Function,Args,Return,Options}]}.

o_o(Handle,Function, Args, Return) ->
  o_o(Handle,Function, Args, Return, ?DEFAULT_ARGS).

o_o(#expectations{stub=S}=Handle,Function, Args, Return, Options) ->
  Handle#expectations{stub = [{Function,Args,Return,Options ++ [{max_invocations,1}]} | S]}.


stub(Handle,Function, Args, Return) ->
  stub(Handle,Function, Args, Return, ?DEFAULT_ARGS).

stub(#expectations{stub=S}=Handle,Function, Args, Return, Options) ->
  Handle#expectations{stub = [{Function,Args,Return,Options} | S]}.

invoke(Handle,Function, Args) ->
  case invoke_strict(Handle,Function,Args) of
    not_found -> 
      case invoke_stub(Handle,Function,Args) of
        not_found -> throw({erlymock,no_match,[{mfa,Function,Args},{state,Handle}]});
        Any -> Any
      end;
    Any -> Any
  end.

invoke_strict(#expectations{strict=[{Func,Args,Return,_Options} | T]}=Handle,CalledFunction,CalledArgs) when Func =:= CalledFunction ->
  RV=case match_args(Args,CalledArgs) of
    true -> Ret=return(Return),
            {Ret,Handle#expectations{strict=T}};
    _ -> not_found
  end,
  RV;
invoke_strict(_,_,_) ->
  not_found.

invoke_stub(#expectations{stub=Stubs}=Handle,Function,Args) ->
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

match_func({Func,Pattern,Return,Options}=Rec, CalledFunc, CalledArgs) when Func =:= CalledFunc ->
  Invocations=proplists:get_value(invocations,Options,0) +1,
  MaxInvocations = proplists:get_value(max_invocations,Options, -1),
  case match_args(Pattern,CalledArgs) of
    true when MaxInvocations >= Invocations -> 
      {true, {Func,Pattern,Return, [{invocations,Invocations} | proplists:delete(invocations,Options)]}, return(Return)};
    true -> 
      throw({erlymock,too_many_invocations,[{mfa,CalledFunc,CalledArgs},{invocations,Invocations,MaxInvocations}]});
    _ -> {false,Rec,undefined}
  end;
match_func(Rec, _CalledFunc, _CalledArgs) ->
  {false,Rec,undefined}.


match_args(Pattern,CalledArgs) ->
  lists:all(fun({P,A}) ->  (P == '_') or (P == A) end, lists:zip(Pattern,CalledArgs)).


return({return,Val}) -> Val.