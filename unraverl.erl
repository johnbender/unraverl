
-module(unraverl).
-export([parse_transform/2]).

-define(MODS, [filters, partial_application]).
-define(ATTR_ALTERATIONS, [before_exec, after_exec]). 
-define(LANG_ALTERATIONS, [partial_application]).

parse_transform(Form, _options) ->
    AltAttr = attribute_alterations(Form, ?ATTR_ALTERATIONS),
    AltLang = lang_alterations(AltAttr, ?LANG_ALTERATIONS),
    io:format("~p", [AltLang]),
    AltLang.

attribute_alterations(Form, []) -> Form;
attribute_alterations(Form, [Alteration|T]) ->
    Altered = per_attr_alter(Form, find_attribute(Form, Alteration)),
    attribute_alterations(Altered, T).

per_attr_alter(Form, []) -> Form;
per_attr_alter(Form, [{attribute,_,Alteration,{ToBeAltered, With}}|T]) ->
    AlteredForm = module_call(Alteration, [Form, ToBeAltered, With]),
    per_attr_alter(AlteredForm, T).

lang_alterations(Form, []) -> Form;
lang_alterations(Form, [Alteration|T]) -> 
    lang_alterations(module_call(Alteration, [Form]), T).

module_call(Alteration, Args) when is_list(Args) ->
    apply(get_module_for_alteration(Alteration), Alteration, Args).

find_attribute(Form, Name) ->
    [{SType, LineNum, SName, Value} || {SType, LineNum, SName, Value} <- Form, Name == SName].

get_module_for_alteration(Function) ->
    [Module] = [X || X<-?MODS, lists:keymember(Function, 1, X:module_info(exports)) ],
    Module.
    

