-module(gprop_atom).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
	convert_existing/1,
        convert/1
]).

%% Internal
convert_existing(List) ->
        binary_to_existing_atom(items_to_binary(List), ?ENCODE).

convert(List) ->
        binary_to_atom(items_to_binary(List), ?ENCODE).

items_to_binary(List) ->
        Result = lists:map(fun
        	(Atom) when is_atom(Atom) ->
			atom_to_binary(Atom, ?ENCODE);
		(Binary) when is_binary(Binary) ->
			Binary
	end, List),
	iolist_to_binary(Result).
