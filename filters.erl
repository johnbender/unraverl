-module(filters).

-export([before_exec/3]).

 
-define(before_form, {function,LineNum,Original,Arity,
			     [{clause,
			       LineNum,
			       OriginalArgs,
			       [],
			       [{match,LineNum,
				 FilterResultArgs, 
				 {call,LineNum ,{atom, LineNum, Filter},FilterArgs}},
				{call,LineNum,
				 {atom,LineNum, RenamedFunc},
				 RenamedFuncArgs}]}]}).

before_exec(Form, [], _filter) -> Form;
before_exec(Form, [Original|T], Filter) ->
    {function, LineNum, Original, Arity, Clauses} = find_function(Form, Original),
    {function, _linenum, Filter, FilterArity, _clauses} = find_function(Form, Filter),
    RenamedFunc = rename(before, Original, Filter),
    Renamed = {function, LineNum, RenamedFunc, Arity, Clauses},
    Replaced = lists:keyreplace(Original, 3, Form, Renamed),
    Appended = append(Replaced, build_parent_function({Original, Arity}, {Filter, FilterArity}, RenamedFunc, eof_line(Replaced))),
    before_exec(Appended, T, Filter).
    

build_parent_function({Original, Arity}, {Filter, FArity}, RenamedFunc, LineNum) ->
    OriginalArgs = build_args_list_form(Arity, LineNum, "Original"),
    FilterResultArgs = build_var_list_form(Arity, LineNum, "Result"),
    FilterArgs = case {Arity > 1, FArity == 1} of
		     {_, true} ->
			 [build_var_list_form(Arity, LineNum, "Original")];
		     {true, false} ->
			 build_args_list_form(Arity, LineNum, "Original");
		     _ -> throw(invalide_filter_arity)
		 end,
    RenamedFuncArgs = build_args_list_form(Arity, LineNum, "Result"),
    ?before_form.

    
build_args_list_form(Count, LineNum, NameSeed) when is_list(NameSeed)  ->
    [{var, LineNum, list_to_atom(NameSeed ++ to_string(Num))} || Num <- lists:reverse(lists:seq(1, Count))].
    

build_var_list_form(0, LineNum, _nameseed) ->
    {nil, LineNum};

build_var_list_form(Count, LineNum, NameSeed) ->
    VarName = list_to_atom(NameSeed ++ to_string(Count)),
    {cons, LineNum, arg_form(LineNum, VarName), build_var_list_form(Count-1, LineNum, NameSeed)}.
    
    
arg_form(Num, Name) when is_number(Num)->
    {var, Num, Name}.
    
find_function(Form, Name) ->
    [Result] = [{SType, LineNum, SName, Arity, Clauses} || {SType, LineNum, SName, Arity, Clauses} <- Form, Name == SName],
    Result.

rename(Type, Original, Filter) ->
    Name = lists:flatten(io_lib:format("~s_~s_~s" , [to_string(Type), to_string(Original), to_string(Filter)])),
    list_to_atom(Name).


append(Form, Addition) ->
    End = {eof, _} = lists:last(Form),
    lists:delete(End, Form) ++ [Addition, End].
    
eof_line(Form)->
    {eof, LineNum} = lists:last(Form),
    LineNum.

remove_eof(Form) ->
    End = {eof, LineNum} = lists:last(Form),
    lists:delete(End, Form).

add_eof(Form, LineNum) ->
    Form ++ [{eof, LineNum}].
    
to_string(Object) when is_number(Object) ->
    lists:flatten(io_lib:format("~w" , [Object]));

to_string(Object) when is_atom(Object) ->
    atom_to_list(Object); 

to_string(Object) when is_binary(Object) ->
    binary_to_list(Object);

to_string(Object) when is_list(Object) ->
    Object.

to_lower_string(Object) ->
    string:to_lower(to_string(Object)).
