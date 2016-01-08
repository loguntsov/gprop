-module(gprop_namespace).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
	start/1, stop/1
]).

-define(ENCODE, latin1).

-type namespace() :: atom().

-export_type([ namespace/0 ]).

start(Namespace) when is_atom(Namespace) ->
	{ ok, Pid } = gprop_sup:start_child({ { space, Namespace}, { gprop_namespace_server, start_link, [ Namespace ] }, permanent, 5000, worker, [ gprop_namespace_server ]}),
	pg2:create({ gprop, Namespace, servers}),
	{ ok, Pid }.

stop(Namespace) when is_atom(Namespace) ->
	gprop_sup:stop_child({ space, Namespace}).

ets_keys_temp(Namespace) ->
	gprop_atom:convert_existing([ Namespace, <<"_keys_temp">>]).

ets_keys_commited(Namespace) ->
	gprop_atom:convert_existing([ Namespace, <<"_keys_commited">>]).

ets_pids(Namespace) ->
	gprop_atom:convert_existing([ Namespace, <<"_pids">>]).


