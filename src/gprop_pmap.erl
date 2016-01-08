-module(gprop_pmap).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
	pmap/3,
]).

-record(state, {
	ref = Ref,
	count = 0,
	result = [],
	timeout = 5000,
	args = []
}).

pmap(Fun, List, Timeout) when is_function(Fun, 1), is_list(List) ->
	Ref = make_ref(),
	Pid = self(),
	Count = length(List),
	lists:foreach(fun({ Number, Item }) ->
		spawn_link(fun() ->
	                Result = Fun(Item),
	                Pid ! { result, Ref, Number, Result }
		end)
	end, lists:zip(lists:seq(1, Count), List)),
        loop(#state{
        	ref = Ref,
	        count = Count,
	        result = [],
	        timeout = Timeout,
	        args = [ Fun, List, Timeout ]
        }).

loop(#state{ count = 0, result = Result }) ->
	{ _, Result } = lists:unzip(lists:keysort(1, Result)),
	Result;
loop(State) ->
	receive
		{ result, R, Number, Result } when R =:= State#state.ref ->
                        loop(State#state{
                        	count = State#state.count - 1,
                        	result = [{Number, Result} | State#state.result ]
                        })
		after State#state.timeout ->
			error(timeout, State#state.args)
	end.
