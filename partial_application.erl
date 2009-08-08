-module(partial_application).
-export([partial_application/1]).

-define(compare_fun, fun({call, _, {atom, _, FuncName}, ArgsList}) ->
			     is_partial(Form, FuncName, ArgsList);
			(_) -> false end).

-define(replace_fun, fun({call, _, {atom, _, FuncName}, ArgsList} = Tmp) ->
			     RealArity = least_arity(util:find_greater_arity(Form, FuncName, ArgsList)),
			     ArityDiff = RealArity - length(ArgsList),
			     io:format("~p ~p ~n", [RealArity, length(ArgsList)]),
			     Tmp end).
%% figure out why least_arity is returning 0			     
			     

partial_application(Form) ->
    util:replace(Form, ?compare_fun, ?replace_fun).

is_partial(Form, FuncName, ArgsList) ->
    case util:find_greater_arity(Form, FuncName, length(ArgsList)) of
	[] -> false;
	_ -> true
    end.
	    
least_arity(Funcs) ->
    least_arity(Funcs, 0).

least_arity([{_, _, _, Arity, _}|T], Max) when Arity >= Max ->
    least_arity(T, Arity);

least_arity([{_, _, _, Arity, _}|T], Max) when Arity < Max ->
    least_arity(T, Max);

least_arity([], Max) -> Max.
			     
