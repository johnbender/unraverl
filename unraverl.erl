
-module(unraverl).
-export([parse_transform/2]).

-define(MODS, [filters]).

parse_transform(Form, _options) ->
    BeforeFilters = alter(Form, find_attribute(Form, before_exec)),
    BeforeFilters.

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
    
