%% @author jason
%% @copyright jason Jan 22, 2010  PROPRIETARY-- NOT FOR DISTRIBUTION
%% @doc TODO: Add description to erlymock
%% @end
%% --------------------------------------------------------------------
-module(erlymock).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% public interface
-export([start/0,strict/3, o_o/3, stub/3,strict/4, o_o/4, stub/4, replay/0, verify/0]).

% interfaces used by other mocking groups, not for consumption
-export([dispatch/1, invocation_event/2,internal_strict/3,internal_stub/3, internal_invocation_event/2,internal_register/1]).

-define(SERVER,?MODULE).

-record(state, {recorder, module_set,state=init,listeners=[]}).

% --------------------------------------------------------------------
%% @spec start() -> {ok, Pid::pid()}
%% @doc Starts the mock process.
%% @end
% --------------------------------------------------------------------
start() ->
  catch(gen_server:call(?SERVER,{reset})),  % make sure a stail instance isn't hanging around
  gen_server:start_link({local,?SERVER},?MODULE,[],[]).

% --------------------------------------------------------------------
%% @spec strict(Module::atom(), Function::atom(), Args::list(term())) -> ok
%% @doc Adds a function to the set of calls that must be called in strict order.  Uses
%% the default options [{return,ok}].
%% @end
% --------------------------------------------------------------------
strict(M,F,Args) when is_atom(M),is_atom(F),is_list(Args)->
  strict(M,F,Args,[{return,ok}]).

% --------------------------------------------------------------------
%% @spec strict(Module::atom(), Function::atom(), Args::list(term()), Options::option_list()) -> ok
%% @doc Adds a function to the set of calls that must be called in strict order.
%% @end
% --------------------------------------------------------------------
strict(M,F,Args, Options) when is_atom(M),is_atom(F),is_list(Args),is_list(Options)->
  dispatch(gen_server:call(?SERVER,{strict,{function,{M,F,length(Args)}},Args,Options})).

% --------------------------------------------------------------------
%% @spec o_o(Module::atom(),Function::atom(),Args::list(term())) -> ok
%% @doc Adds an out-of-order call with default options.  Equivalent to
%% stub(Module,Function,Args,[{return,ok},{max_invocations,1}]).
%% @end
% --------------------------------------------------------------------
o_o(M,F,Args) when is_atom(M),is_atom(F),is_list(Args)->
  stub(M,F,Args,[{return,ok}]).

% --------------------------------------------------------------------
%% @spec o_o(Module::atom(),Function::atom(),Args::list(term()),Options::option_list()) -> ok
%% @doc Adds an out-of-order call.  Equivalent to
%% stub(Module,Function,Args,[{max_invocations,1} | Options]).
%% @end
% --------------------------------------------------------------------
o_o(M,F,Args, Options) when is_atom(M),is_atom(F),is_list(Args),is_list(Options)->
  stub(M,F,Args,[{max_invocations,1},{min_invocations,1} | Options]).


% --------------------------------------------------------------------
%% @spec stub(Module::atom(),Function::atom(),Args::list(term())) -> ok
%% @doc Adds a stub call. 
%% @end
% --------------------------------------------------------------------
stub(M,F,Args) when is_atom(M),is_atom(F),is_list(Args)->
  stub(M,F,Args,[{return,ok}]).

% --------------------------------------------------------------------
%% @spec stub(Module::atom(),Function::atom(),Args::list(term()),Options::option_list()) -> ok
%% @doc Adds a stub call. 
%% @end
% --------------------------------------------------------------------
stub(M,F,Args, Options) when is_atom(M),is_atom(F),is_list(Args), is_list(Options)->
  dispatch(gen_server:call(?SERVER,{stub,{function,{M,F,length(Args)}},Args,Options})).

% --------------------------------------------------------------------
%% @spec replay() -> ok
%% @doc Marks the end of initialization and the begin of the call playback.
%% @end
% --------------------------------------------------------------------
replay() ->
  dispatch(gen_server:call(?SERVER,{replay})).

% --------------------------------------------------------------------
%% @spec verify() -> ok | Error::term()
%% @doc Confirms that the mocked calls were made properly with respect to their
%% parameters.  
%% @end
% --------------------------------------------------------------------
verify() ->
  dispatch(gen_server:call(?SERVER,{verify})).

% --------------------------------------------------------------------
%% @spec invocation_event(MFA,Args) -> RV::term()
%% @private
%% @doc Used by the generated code to stub out the overriden modules.  Not intended for general use.
%% @end
% --------------------------------------------------------------------
invocation_event(MFA,Args) when is_tuple(MFA), is_list(Args)->
  dispatch(gen_server:call(?SERVER,{invocation_event,{function,MFA},Args})).

  
% --------------------------------------------------------------------
%% @spec dispatch(What) -> any()
%% @private
%% @doc Returns, throws, errors, etc based upon What.  Used internally
%% for moving values onto the calling process's stack
%% @end 
% --------------------------------------------------------------------
dispatch(What) ->  
  case What of
    {throw,T} -> throw(T);
    {exit,E} -> exit(E);
    {error,E} -> erlang:error(E);
    {ok,RV} -> RV;
    Any -> Any
  end.

internal_strict(Func,Args,Options) ->
  dispatch(gen_server:call(?SERVER,{strict,Func,Args,Options})).

internal_stub(Func,Args,Options) ->
  dispatch(gen_server:call(?SERVER,{stub,Func,Args,Options})).

internal_invocation_event(Func,Args) ->
  gen_server:call(?SERVER,{invocation_event,Func,Args}).

internal_register(AssocPid) ->
  gen_server:call(?SERVER,{register,AssocPid}).

% --------------------------------------------------------------------
%% @spec init([]) ->
%%          {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% @private
%% @doc Initiates the server
%% @end 
% --------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit,true),
  {ok, #state{recorder=erlymock_recorder:new(),module_set=sets:new()}}.

% --------------------------------------------------------------------
%% @spec handle_call(Request::term(), From::pid(), State::state()) ->
%%          {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   |
%%          {stop, Reason, State}
%% @private
%% @doc Handling call messages
%% @end
% --------------------------------------------------------------------
handle_call({strict,Func,Args,Options}, _From, #state{recorder=Rec,state=init}=State) ->
  Rec2=erlymock_recorder:strict(Rec, Func, Args, Options),
  {reply, ok, State#state{recorder=Rec2}};
handle_call({strict,_F,_Args,_Options}, _From, #state{state=S}=State) ->
  {reply, {throw,{invalid_state,S}}, State};

handle_call({stub,Func,Args,Options}, _From, #state{recorder=Rec,state=init}=State) ->
  Rec2=erlymock_recorder:stub(Rec, Func, Args, Options),
  {reply, ok, State#state{recorder=Rec2}};
handle_call({stub,_F,_Args,_Options}, _From, #state{state=S}=State) ->
  {reply, {throw,{invalid_state,S}}, State};

handle_call({replay}, _From, #state{listeners=Listeners,recorder=Rec,state=init}=State) ->
  true=lists:all(fun(L) -> gen_server:call(L,{erlymock_state,replay}) end, Listeners),
  ModSet=make_mock_modules(Rec),
  {reply,ok,State#state{state=replay,module_set=ModSet}};
handle_call({replay}, _From, #state{state=S}=State) ->
  {reply, {throw,{invalid_state,S}}, State};


handle_call({verify}, _From, #state{listeners=Listeners,recorder=Rec,state=replay}=State) ->
  ListProblems = lists:foldl(
                  fun(L,Acc) -> 
                       case gen_server:call(L,{erlymock_state,verify}) of
                         true -> Acc;
                         Any -> [Any | Acc]
                       end
                  end, [], Listeners),
  case erlymock_recorder:validate_constraints(Rec) of
    [] -> {stop,normal,ok,State#state{state=done}};
    Problems -> {stop,normal, {throw,{mock_failure,ListProblems ++ Problems}}, State}
  end;
handle_call({verify}, _From, #state{state=S}=State) ->
  {stop, normal, {throw,{invalid_state,S}}, State};

handle_call({reset}, _From, State) ->
  {stop,normal,ok,State};

handle_call({invocation_event,Func,Args}, _From, #state{recorder=Rec,state=replay}=State) ->
  try erlymock_recorder:invoke(Rec,Func,Args) of
    {RV,R2} -> {reply,{ok,RV},State#state{recorder=R2}}
  catch
    throw:RV -> {reply,{throw,RV},State};
    exit:Reason -> {reply,{exit,Reason},State};
    error:Reason ->{reply,{error,Reason},State}
  end;
handle_call({invocation_event,_Func,_Args}, _From, #state{state=S}=State) ->
  {reply,{throw,{invalid_state,S}},State};

handle_call({register,ListenerPid},_From,#state{listeners=Listeners,state=init}=State) ->
  {reply,ok,State#state{listeners=[ListenerPid | Listeners]}};
handle_call({register,_ListenerPid},_From,#state{state=S}=State) ->
  {reply,{cannot_register_in_state,S},State};


handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% --------------------------------------------------------------------
%% @spec handle_cast(Msg::term(), State::state()) ->
%%          {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%% @private
%% @doc Handling cast messages
%% @end
% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
%% @spec handle_info(Info::term(), State::state()) ->
%%          {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%% @private
%% @doc Handling all non call/cast messages
%% @end
% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
%% @spec terminate/2(Reason::term(),State::state()) -> any()
%% @private
%% @doc Shutdown the server
%% @end
% --------------------------------------------------------------------
terminate(_Reason, #state{module_set=ModuleSet}) ->
  lists:map(fun(Mod) ->
                 code:purge(Mod),
                 code:delete(Mod)
            end, sets:to_list(ModuleSet)).

% --------------------------------------------------------------------
%% @spec code_change(OldVsn,State,Extra) -> {ok, NewState}
%% @private
%% @doc Convert process state when code is changed
%% @end
% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


make_mock_modules(Rec) ->
  {ModSet,FuncSet}=erlymock_recorder:foldl(
                    fun({{function,{M,_F,_A}=Mfa},_,_},{Mods,Funcs}) -> {sets:add_element(M,Mods),sets:add_element(Mfa,Funcs)};
                        (_,Acc) -> Acc end,
                          {sets:new(),sets:new()}, Rec),
  sets:fold(fun(Mod,_) -> 
                make_module(Mod,sets:filter(fun({M,_,_}) when M =:= Mod-> true ; (_) -> false end, FuncSet)),
                nill % using fold as foreach, ignoring accumulator
           end, nill, ModSet),
  ModSet.
                

make_module(Mod,FuncSet) ->
  HeaderForm = module_header_abstract_form(Mod),
  FunctionForms = sets:fold(
                   fun({M,F,A},FFAcc) when M =:=Mod-> 
                        [fundef_to_abstract_meta_form(M, F, A)|FFAcc]
                   end,
                   [],
                   FuncSet),
  compile_and_load_abstract_form(HeaderForm ++ FunctionForms).

module_header_abstract_form(Mod) ->
  [{attribute,0,module,Mod}, 
   {attribute,0,compile,[export_all]}].

fundef_to_abstract_meta_form(Mod, FunName, Arity) ->
  Line = 1,
  Params = [{var, Line, list_to_atom("A" ++ integer_to_list(I))} || I <- seq(1,Arity)],
  {function, Line, FunName, Arity,
   [{clause, Line, 
     Params, [], 
     [{call, Line, 
       {remote, Line, {atom, Line, ?MODULE}, {atom, Line, invocation_event}}, 
       [{tuple, Line, 
         [{atom, Line, Mod}, {atom,Line, FunName}, {integer, Line, Arity}]},
        lists:foldr(
         fun(E,R) ->
              {cons, Line, E, R}
         end, 
         {nil, Line}, 
         Params)]
      }]
    }]
  }.

compile_and_load_abstract_form(AbsForm) ->
  CompRes = compile:forms(AbsForm),
  {ok, Mod, Code} = CompRes,
  code:purge(Mod),
  code:delete(Mod),
  {module, _} = load_module(Mod, Code).

seq(A, E) when A > E -> [];
seq(A, E) -> lists:seq(A,E).

