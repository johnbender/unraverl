-module(partial_application).
-export([partial_application/1]).

-define(compare_fun, fun({call, _, {atom, _, FuncName}, ArgsList}) ->
			     is_partial(Form, FuncName, ArgsList);
			(_) -> false 
		     end).

-define(replace_fun, fun({call, LineNum, {atom, _, FuncName}, ArgsList}) ->
			     RealArity = real_arity(Form, FuncName, ArgsList),
			     ArityDiff = RealArity - length(ArgsList),
			     FunArgList = util:args_list_form(ArityDiff, LineNum, "FunArg"),
			     ?partialy_applied_function
		     end).

-define(partialy_applied_function, {'fun',LineNum,
				    {clauses,
				     [{clause,LineNum,
				       FunArgList, %fun arg list [{var,52,'Arg3'}],
				       [], %guards
				       [{call,LineNum,
					 {atom,LineNum,FuncName},
					 ArgsList ++ FunArgList }]}]}}). %Args to pass [{var,52,'Arg1'}]


%% figure out why least_arity is returning 0			     
			     

partial_application(Form) ->
    util:replace(Form, ?compare_fun, ?replace_fun).

is_partial(Form, FuncName, ArgsList) ->
    Arity = length(ArgsList),
    real_arity(Form, FuncName, ArgsList) > Arity.
	    
least_arity(Funcs) ->
    least_arity(Funcs, 0).

least_arity([{_, _, _, Arity, _}|T], Max) when Arity >= Max ->
    least_arity(T, Arity);

least_arity([{_, _, _, Arity, _}|T], Max) when Arity < Max ->
    least_arity(T, Max);

least_arity([], Max) -> Max.

real_arity(Form, FuncName, ArgsList) ->	
    least_arity(find_greater_equal_arity(Form, FuncName, length(ArgsList))).		     
     

find_greater_equal_arity(Form, Name, Arity) ->
    [{SType, LineNum, SName, SArity, Clauses} || {SType, LineNum, SName, SArity, Clauses} <- Form, Name == SName, Arity =< SArity].
