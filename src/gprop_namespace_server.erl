-module(gprop_namespace_server).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(gen_server).

%% API
-export([
	start_link/1,
	workers/2, local_worker/2,
	pid/1, pids/1, remote_pids/1,
	ets_namespace/1
]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(WORKERS, {1,2,3,4,5,6,7,8,9,10}).

-record(state, {
	namespace :: atom(),
	workers = [] :: [pid()],
	ets :: ets:tab(),
	tracker = gprop_tracker:new() :: gprop_tracker:tracker()
}).

-spec start_link( Namespace :: gprop_namespace:namespace() ) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Namespace) ->
	gen_server:start_link({local, Namespace}, ?MODULE, [ Namespace ], []).

pid(Namespace) when is_atom(Namespace) ->
	whereis(Namespace).

pids(Namespace) ->
	pg2:get_members({ space, Namespace }).

remote_pids(Namespace) ->
	 pids(Namespace) - pid(Namespace).

workers(Namespace, Key) ->
	I = element(phash2(Key, size(?WORKERS)), ?WORKERS),
	pg2:get_members({ gprop, Namespace, I}).

local_worker(Namespace, Key) ->
	I = element(phash2(Key, size(?WORKERS)), ?WORKERS),
	whereis(gprop_atom:convert_existing(group(Namespace, I))).

ets_namespace(Namespace) ->
	gprop_atom:convert_existing([ <<"gprop_namespace_">>, Namespace ]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([ Namespace ]) ->
	pg2:join({gprop, Namespace}, self()),
	ok = gprop_namespace_local:init(Namespace),
	Ets = gprop_atom:convert([ <<"gprop_namespace_">>, Namespace ]),
	Ets = ets:new(Ets, [ named_table, set, public, { write_concurrency, true}, { read_concurrency, true} ]),
	Workers = lists:map(fun(I) ->
		Id = gprop_atom:convert(group(Namespace, I)),
		pg2:create({ gprop, Id}),
		{ ok, Pid } = gprop_namespace_worker:start_link(Namespace, Id),
		pg2:join({ gprop, Namespace, I}, Pid)
	end, tuple_to_list(?WORKERS)),
	State = #state{
		namespace = Namespace,
		ets = Ets,
		workers = Workers
	},
	{ ok, State }.

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
handle_cast(_Request, State) ->
	{noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, State) ->
	pg2:leave({ gprop, State#state.namespace}, self()),
	ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

group(Namespace, I) ->
	[ <<"gprop_">>, Namespace, <<"_g">>, integer_to_binary(I) ].