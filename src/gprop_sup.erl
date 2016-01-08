-module(gprop_sup).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_child/1, stop_child/1,
	set/3, get/2
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(NAMESPACE_ETS, gprop_namespaces_global).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
	{ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
		MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
		[ChildSpec :: supervisor:child_spec()]
	}} |
	ignore |
	{error, Reason :: term()}).
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,
	?NAMESPACE_ETS = ets:new(?NAMESPACE_ETS, [ named_table, set, public, {concurrency_write, true}, { read_concurrency, true } ]),
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	AChild = [],
	{ok, {SupFlags, [AChild]}}.
start_child(Child) ->
	supervisor:start_child(?SERVER, Child).

stop_child(ChildId) ->
	supervisor:terminate_child(?SERVER, ChildId),
	supervisor:delete_child(?SERVER, ChildId).

set(Namespace, Key, Value) ->
	ets:insert(?NAMESPACE_ETS, {{Namespace, Key}, Value}).

get(Namespace, Key) ->
	case ets:lookup(?NAMESPACE_ETS, {Namespace, Key}) of
		[] -> error(not_exists, [ Namespace, Key ]);
		[{_, Value}] -> Value
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
