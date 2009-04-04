
-module(unraverl).
-export([parse_transform/2]).

-define(MODS, [filters]).
-define(ALTERATIONS, [before_exec, after_exec]). 

parse_transform(Form, _options) ->
    perform_alterations(Form, ?ALTERATIONS).

perform_alterations(Form, []) -> Form;
perform_alterations(Form, [Alteration|T]) ->
    Altered = alter(Form, find_attribute(Form, Alteration)),
    perform_alterations(Altered, T).
    
alter(Form, []) -> Form;
alter(Form, [{attribute,_,Alteration,{ToBeAltered, With}}|T]) ->
    Module = get_module_for_alteration(Alteration),
    AlteredForm = Module:Alteration(Form,ToBeAltered, With),
    alter(AlteredForm, T).

find_attribute(Form, Name) ->
    [{SType, LineNum, SName, Value} || {SType, LineNum, SName, Value} <- Form, Name == SName].

get_module_for_alteration(Function) ->
    [Module] = [X || X<-?MODS, lists:keymember(Function, 1, X:module_info(exports)) ],
    Module.
    
