-module(gprop_namespace_worker).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(gen_server).

%% API
-export([
	start_link/2,
	track/3, untrack/3
]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {
	namespace :: gprop_namespace:namespace(),
	id :: atom(),
	ets :: ets:tab()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Namespace, Id) ->
	gen_server:start_link({ local, Id }, ?MODULE, [ Namespace, Id ], []).

track(Namespace, Type, Atom) when is_atom(Atom) ->
	track(Namespace, Type, whereis(Atom));
track(Namespace, l, Pid) when is_pid(Pid) ->
	Worker = gprop_namespace_server:local_worker(Namespace, Pid),
	Ets = gprop_namespace_server:ets_namespace(Namespace),
	case ets:lookup(Ets, { Worker, Pid }) of
		[] ->
			cast(Worker, { track, Pid });
		[_] ->
                        ok
	end.

untrack(Namespace, Type, Atom) when is_atom(Atom) ->
	untrack(Namespace, Type, whereis(Atom));
untrack(Namespace, l, Pid) when is_pid(Pid) ->
	Worker = gprop_namespace_server:local_worker(Namespace, Pid),
	Ets = gprop_namespace_server:ets_namespace(Namespace),
	case ets:lookup(Ets, { Worker, Pid }) of
		[] -> ok;
		[_] ->
			cast(Worker, { untrack, Pid })
	end.

spread(Namespace, Key, Pid, Phase) ->
	Workers = gprop_namespace_server:workers(Namespace),
	gen_server:multi_call()




%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([ Namespace, Id ]) ->
	Ets = gprop_namespace_server:ets_namespace(Namespace),
	{ok, #state{
		namespace = Namespace,
		id = Id,
		ets = Ets
	}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast({track, Pid}, State) ->
	Ets = State#state.ets,
	case ets:insert_new(Ets, {{self(), Pid}, undefined}) of
		true ->
			Ref = monitor(process, Pid),
			ets:insert(Ets, {{self(), Pid}, Ref});
		false -> ok
	end,
	{ noreply, State };
handle_cast({untrack, Pid}, State) ->
	Ets = State#state.ets,
	case ets:lookup(Ets, {self(), Pid}) of
		[{_, Ref}] ->
			ets:delete(Ets, {self(), Pid}),
			demonitor(Ref);
		[] -> ok
	end,
	{ noreply, State };

handle_cast(_Request, State) ->
	{noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
	gprop_namespace_local:clear_pid(State#state.namespace, l, Pid),
	gprop_namespace_local:clear_pid(State#state.namespace, g, Pid),
	{ noreply, State };
handle_info(_Info, State) ->
	{noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cast(Pid, Msg) ->
	gen_server:cast(Pid, Msg).