-module(util).

-export([find_function/2, replace/3, replace/2]). 

-define(call_check, fun({call,_,{atom,_,Name},_}) -> true;
                       (_) -> false
                    end).

-define(function_check, fun({_, _, Name, _, _}) -> true;
                           (_) -> false
                        end).


find_function(Form, Name) ->
    [Result] = [{SType, LineNum, SName, Arity, Clauses} || {SType, LineNum, SName, Arity, Clauses} <- Form, Name == SName],
    Result.


%If the fun can handle both matching and replacement
replace(Form, ReplaceFun) when is_tuple(Form) ->
    map_tuple(ReplaceFun(Form), ?MODULE, replace, [ReplaceFun]);

replace(Form, ReplaceFun) when is_list(Form) ->
    [replace(X, ReplaceFun) || X <- ReplaceFun(Form)];

replace(Form, ReplaceFun) ->
    ReplaceFun(Form).


				  
%If the third argument is not a replacement fun but a data structure
replace(Form, CompareFun, Replacement) when is_function(Replacement) == false ->
    replace(Form, CompareFun, fun(_) -> Replacement end);

%If the matching and replacement funs are separated
replace(Form, CompareFun, ReplaceFun) when is_tuple(Form) ->
    case CompareFun(Form) of 
	true -> ReplaceFun(Form);
	_ -> map_tuple(Form, ?MODULE, replace, [CompareFun, ReplaceFun])
    end;

replace(Form, CompareFun, ReplaceFun) when is_list(Form) ->
    case CompareFun(Form) of 
	true -> ReplaceFun(Form);
	_ -> [replace(X, CompareFun, ReplaceFun) || X <- Form]
    end;

replace(Form, CompareFun, ReplaceFun) ->
    case CompareFun(Form) of
	true -> ReplaceFun(Form);
	_ -> Form
    end.


map_tuple(Tuple, Module, Function, Args) when is_tuple(Tuple) -> 
    map_tuple(tuple_to_list(Tuple), Module, Function, Args);

map_tuple([Head|T], Module, Function, Args) ->
    map_tuple(T, Module, Function, Args, { apply(Module, Function, [Head|Args]) }).

map_tuple([Head|T], Module, Function, Args, Acc) ->
    Tuple = erlang:append_element(Acc, apply(Module, Function, [Head|Args])),
    map_tuple(T, Module, Function, Args, Tuple);

map_tuple([], _, _, _, Acc) -> Acc.

    
