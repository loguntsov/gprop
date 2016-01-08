-module(gprop_tracker).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
	new/0,
	add/3,
	delete/2,
	is_tracked/2
]).

-type item() :: {atom(), pid(), reference() }.
-type tracker() :: [item()].

-export_type([ tracker/0 ]).

new() -> [].

add(What, Pid, Tracker) when is_atom(What), is_pid(Pid), is_list(Tracker) ->
	Ref = monitor(process, Pid),
	[{What, Pid, Ref} | Tracker].

delete(Ref, Tracker) when is_reference(Ref), is_list(Tracker) ->
	lists:keydelete(Ref, 3, Tracker);

delete(Pid, Tracker) when is_pid(Pid), is_list(Tracker) ->
	lists:keydelete(Pid, 2, Tracker).

is_tracked(What, Tracker) ->
	lists:any(fun({ W, _, _}) when W =:= What -> true;(_) -> false end, Tracker).


