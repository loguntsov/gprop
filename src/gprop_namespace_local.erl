-module(gprop_namespace_local).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
        init/1,
        destroy/1,
        add_property/5,
        add_uniq_property/5,
	del_property/4,
	lookup_property/3,
	lookup_pid/3,
	clear_pid/3
]).

init(Namespace) ->
	EtsData = gprop_atom:convert([ Namespace, <<"_data">>]),
	EtsData = ets:new(EtsData, [ named_table, set, public, { write_concurrency, true }, {read_concurrency, true} ]),
	EtsPids = gprop_atom:convert([ Namespace, <<"_pids">>]),
	EtsPids = ets:new(EtsPids, [ named_table, bag, public, { write_concurrency, true }, {read_concurrency, true} ]),
        ok.

destroy(Namespace) ->
	Ets = ets_data(Namespace),
	ets:delete(Ets),
	ok.

add_property(Namespace, Type, Key, Value, Pid) ->
	EtsData = ets_data(Namespace),
        case ets:insert_new(EtsData, {{Type, Key, Pid}, Value }) of
		true ->
			EtsPids = ets_pids(Namespace),
        		ets:insert(EtsPids, {{Type, k, Key}, Pid }),
        		ets:insert(EtsPids, {{Type, p, Pid}, Key }),
        		ok;
		false ->
			{ error, already_exists }
	end.

add_uniq_property(Namespace, Type, Key, Value, Pid) ->
	EtsPids = ets_pids(Namespace),
	case ets:insert_new(EtsPids, {{Type, k, Key}, Pid }) of
		true ->
			ets:insert_new(EtsPids, {{Type, p, Pid}, Key}),
			EtsData = ets_data(Namespace),
			ets:insert(EtsData, {{Key, Pid}, Value}),
			ok;
		false ->
			{ error, already_exists }
	end.

del_property(Namespace, Type, Key, Pid) ->
	EtsPids = ets_pids(Namespace),
	EtsData = ets_data(Namespace),
	ets:delete_object(EtsPids, {{ Type, k, Key}, Pid }),
	ets:delete_object(EtsPids, {{ Type, p, Pid}, Key }),
	ets:delete(EtsData, { Type, Key, Pid }).

lookup_property(Namespace, Type, Key) ->
	EtsPids = ets_pids(Namespace),
	EtsData = ets_data(Namespace),
	Keys = ets:lookup(EtsPids, {Type, k, Key}),
	lists:foldl(fun({_, Pid}, Acc) ->
	        case ets:lookup(EtsData, {Key, Pid}) of
			[] -> Acc;
			[{_, Value}] -> [{Pid, Value}|Acc]
		end
	end, [], Keys).

lookup_pid(Namespace, Type, Pid) ->
	EtsPids = ets_pids(Namespace),
	EtsData = ets_data(Namespace),
	Keys = ets:lookup(EtsPids, {Type, p, Pid}),
	lists:foldl(fun({_, Key}, Acc) ->
	        case ets:lookup(EtsData, {Key, Pid}) of
			[] -> Acc;
			[{_, Value}] -> [{Key, Value}|Acc]
		end
	end, [], Keys).

clear_pid(Namespace, Type, Pid) ->
	EtsPids = ets_pids(Namespace),
	EtsData = ets_data(Namespace),
	Keys = ets:lookup(EtsPids, {Type, p, Pid}),
	lists:foldl(fun({_, Key}, Acc) ->
		ets:delete_object(EtsPids, {{ Type, k, Key}, Pid }),
		ets:delete_object(EtsPids, {{ Type, p, Pid}, Key }),
		ets:delete(EtsData, { Type, Key, Pid })
	end, [], Keys).


%% Internal

ets_data(Namespace) ->
	gprop_atom:convert_existing([ Namespace, <<"_data">>]).

ets_pids(Namespace) ->
	gprop_atom:convert_existing([ Namespace, <<"_pids">> ]).
