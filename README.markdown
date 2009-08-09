Unraverl
=======

Unraverl is a parse transform library for erlang. You can run your module through the 
transformation by including the following attribute at the top of your module:

    -compile([{parse_transform, unraverl}]).

Its rough around the edges and has no testing or build scripts at this point, 
but feel free to play around.

This parse transform provides two facilities:

Filters
-------

By defining filters as attributes at the beginning of your module, unraverl will create
a function chain for you, removing the need for extra calls inside the filtered functions.

In the following example both prepend and append have the all_lists function applied as
a before filter to make sure that all arguments passed are lists. Additionally, the 
to_atom function is applied as an after filter to make sure that the result is an atom.
  
    -before_exec({[prepend, append], all_lists}).
    -after_exec({[prepend, append], to_a_atom}).

    prepend(S1, S2) ->
        S1 ++ S2.

    append(S1, S2) ->
        S2 ++ S1.

    all_lists(Objects) when is_list(Objects)->
        lists:map(fun to_list/1, Objects).

    to_list(Object) when is_number(Object) ->
        lists:flatten(io_lib:format("~w" , [Object]));

    to_list(Object) when is_atom(Object) ->
        atom_to_list(Object); 

    to_list(Object) when is_binary(Object) ->
        binary_to_list(Object);

    to_list(Object) when is_tuple(Object) ->
        tuple_to_list(Object); 

    to_list(Object) when is_list(Object) ->
        Object.

    to_atom(List) when is_list(List) ->
        Result = list_to_atom(lists:flatten(List)),
        Result.

Obviously since we aren't able to modify global state with our filters they must alter the 
incoming arguments and as a result there are constraints on the arity of the filter function. 
Either the arity must match that of the function being filtered or it must take exactly one 
argument as a list of the arguments being passed.

Partial Application
-------------------

Whenever a local function call is made within a module where the arity of the call is less
than the next highest arity function definition, the call will be replaced by a fun definition
requiring the remaining arguments.

    add(L, R) -> L + R. 

    foo() -> add(1). 

In most cases this would give you a compile error, but unraverl replaces foo with.

    foo() -> fun(R) -> add(1, R) end.

Cheers!
