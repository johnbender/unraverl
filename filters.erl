-module(filters).
-export([before_exec/3, after_exec/3]).
 
-define(before_form, {function,LineNum,Original,Arity,
		      [{clause,
			LineNum,
			OriginalArgs,
			[],
			[{match,LineNum,FilterResultArgs, 
			  {call,LineNum ,{atom, LineNum, Filter},FilterArgs}},
			 {call,LineNum,
			  {atom,LineNum, RenamedFunc},
			  RenamedFuncArgs}]}]}).

-define(after_form, {function,LineNum,Original,Arity,
		     [{clause, LineNum,
		       OriginalArgs,
		       [],
		       [{match,LineNum,
			 RenamdFuncResultArg,
			 {call,LineNum,{atom,LineNum, RenamedFunc}, OriginalArgs}},
			{call,LineNum,{atom,LineNum,Filter},FilterArgs}]}]}).


before_exec(Form, Funcs, Filter) ->
    Fun = fun(A, B, C, D) -> before_exec_function(A, B, C, D) end,
    apply_alteration(Form, Funcs, Filter, Fun).

after_exec(Form, Funcs, Filter) ->
    Fun = fun(A, B, C, D) -> after_exec_function(A, B, C, D) end,
    apply_alteration(Form, Funcs, Filter, Fun).

apply_alteration(Form, [], _filter, _alteration) -> Form;
apply_alteration(Form, [Original|T], Filter, FunctionAddition) ->
    {function, LineNum, Original, Arity, Clauses} = util:find_function(Form, Original),
    {function, _linenum, Filter, FilterArity, _clauses} = util:find_function(Form, Filter),
    RenamedFunc = rename(before, Original, Filter),
    Renamed = {function, LineNum, RenamedFunc, Arity, Clauses},
    Replaced = lists:keyreplace(Original, 3, Form, Renamed),
    Appended = append(Replaced, FunctionAddition({Original, Arity}, {Filter, FilterArity}, RenamedFunc, eof_line(Replaced))),
    apply_alteration(Appended, T, Filter, FunctionAddition).
    

before_exec_function({Original, Arity}, {Filter, FArity}, RenamedFunc, LineNum) ->
    OriginalArgs = util:args_list_form(Arity, LineNum, "Original"),
    FilterResultArgs = var_list_form(Arity, LineNum, "Result"),
    FilterArgs = case {Arity == FArity, FArity == 1} of
		     {true, _} ->
			 util:args_list_form(Arity, LineNum, "Original");
		     {_, true} ->
			 [var_list_form(Arity, LineNum, "Original")];
		     _ -> throw(invalide_filter_arity)
		 end,
    RenamedFuncArgs = util:args_list_form(Arity, LineNum, "Result"),
    ?before_form.

after_exec_function({Original, Arity}, {Filter, FArity}, RenamedFunc, LineNum) ->
    OriginalArgs = util:args_list_form(Arity, LineNum, "Original"),
    RenamdFuncResultArg = var_form(LineNum, "Result1"),
    FilterArgs = case FArity == 1 of
		     true ->
			 util:args_list_form(1, LineNum, "Result");
		     false -> throw(invalide_filter_arity)
		 end,
    ?after_form.
    
var_list_form(0, LineNum, _nameseed) ->
    {nil, LineNum};

var_list_form(Count, LineNum, NameSeed) ->
    VarName = list_to_atom(NameSeed ++ util:to_string(Count)),
    {cons, LineNum, var_form(LineNum, VarName), var_list_form(Count-1, LineNum, NameSeed)}.
    
    
var_form(Num, Name) when is_list(Name) ->
    var_form(Num, list_to_atom(Name));

var_form(Num, Name) when is_number(Num) ->
    {var, Num, Name}.

rename(Type, Original, Filter) ->
    Name = lists:flatten(io_lib:format("~s_~s_~s" , [util:to_string(Type), util:to_string(Original), util:to_string(Filter)])),
    list_to_atom(Name).


append(Form, Addition) ->
    End = {eof, _} = lists:last(Form),
    lists:delete(End, Form) ++ [Addition, End].
    
eof_line(Form)->
    {eof, LineNum} = lists:last(Form),
    LineNum.
    

