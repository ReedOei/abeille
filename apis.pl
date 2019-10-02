#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(library(clpfd)).
:- use_module(library(filesex)).
:- use_module(library(achelois)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(url)).

root(Root) :-
    source_file(File),
    atom_concat(_, 'apis.pl', File),
    file_directory_name(File, Root).

main(Argv) :-
    (
        Argv = [TopName|_] -> true;
        TopName = 'top'
    ),

    gen_structures(TopName).

gen_structures(TopName) :-
    prompt(_, ''),
    json_read(user_input, Result),
    build_structure(Result, TopName, Structure),
    generate_accessors(Structure, Accessors),
    generate_parsers(Structure, Parsers),
    forall(member(Accessor, Accessors), format('~w.~n', Accessor)),
    writeln(''),
    forall(member(Parser, Parsers), format('~w.~n', Parser)).

generate_parsers(structure(Name, Attrs), [Parser|Parsers]) :-
    maplist(key, Attrs, Keys),
    StructureFunctor =.. [Name|Keys],
    atomic_list_concat(['parse', Name], '_', ParserName),
    ParserFunctor =.. [ParserName, json('Json'), StructureFunctor],
    maplist(generate_field_parsers, Attrs, FieldParsersList, RestParsers),
    flatten(FieldParsersList, FieldParsers),
    build_body(FieldParsers, Body),
    Parser =.. [':-', ParserFunctor, Body],
    flatten(RestParsers, Parsers).
generate_parsers(listof(S), Parsers) :-
    generate_parsers(S, Parsers).
generate_parsers(Other, []) :- % only generate parsers for structures/lists
    Other \= structure(_, _),
    not(is_list(Other)).

build_body([Parser], Parser).
build_body([Parser|Parsers], Res) :-
    build_body(Parsers, Temp),
    Res =.. [',', Parser, Temp].

call_parser(structure(Name, _Attrs), KeyName, KeyUpper, [ParserFunctor]) :-
    atomic_list_concat(['parse', Name], '_', ParserName),
    ParserFunctor =.. [ParserName, KeyName, KeyUpper].
call_parser(listof(S), KeyName, KeyUpper, ParserCalls) :-
    call_parser(S, KeyName, KeyUpper, Calls),
    (
        Calls = [Call] ->
            Call =.. [CallPred|_],
            ParserCalls = [maplist(CallPred, KeyName, KeyUpper)];

        ParserCalls = []
    ).
call_parser(Other, _, _, []) :- % only generate parsers for structures/lists
    Other \= structure(_, _),
    not(is_list(Other)).

generate_field_parsers(Key=Val, [member(KV, 'Json')|ParserCalls], Parsers) :-
    generate_parsers(Val, Parsers),

    key(Key=_, KeyUpper),

    (
        Val = structure(_,_) -> atom_concat('Temp', KeyUpper, KeyName);
        Val = listof(_) -> atom_concat('Temp', KeyUpper, KeyName);

        KeyName = KeyUpper
    ),

    KV =.. ['=', Key, KeyName],

    call_parser(Val, KeyName, KeyUpper, ParserCalls).

generate_accessors(structure(Name, Attrs), Accessors) :-
    maplist(key, Attrs, Keys),
    maplist(generate_accessor(Name, Keys), Attrs, StructureAccessors, OtherAccessorsLists),
    flatten([StructureAccessors|OtherAccessorsLists], Accessors).
generate_accessors(listof(S), Accessors) :-
    generate_accessors(S, Accessors).
generate_accessors(Other, []) :- % only generate accessors for structures/lists
    Other \= structure(_, _),
    not(is_list(Other)).

title_case(A, TitleA) :-
    atom_concat(First, Rest, A),
    atom_codes(First, [_]), % We just want the first character
    upcase_atom(First, FirstUpper),
    atom_concat(FirstUpper, Rest, TitleA).

key(Key=_Val, KeyUpper) :-
    atomic_list_concat(Parts, '_', Key),
    maplist(title_case, Parts, UpperParts),
    atomic_list_concat(UpperParts, '', KeyUpper).

generate_accessor(Name, Keys, Key=Val, AccessorFunctor, ValAccessors) :-
    StructureFunctor =.. [Name|Keys],
    key(Key=Val, KeyUpper),
    atomic_list_concat([Name, Key], '_', AccessorName),
    AccessorFunctor =.. [AccessorName, StructureFunctor, KeyUpper],
    generate_accessors(Val, ValAccessors).

type_of(@(true), bool).
type_of(@(false), bool).
type_of(X, int) :- integer(X).
type_of(X, str) :- atom(X).
type_of(@(null), any).

build_structure(X, _, literal(Type)) :- type_of(X, Type).
build_structure(json(InnerJson), Name, Structure) :-
    build_structure(InnerJson, Name, Structure).

build_structure([], Name, structure(Name, [])).
build_structure([Key=json(InnerJson)|Rest], Name, structure(Name, [Key=NewStructure|Attrs])) :-
    build_structure(InnerJson, Key, NewStructure),
    build_structure(Rest, Name, structure(Name, Attrs)).

build_structure([Key=[H|_]|Rest], Name, structure(Name, [Key=listof(NewStructure)|Attrs])) :-
    build_structure(H, Key, NewStructure),
    build_structure(Rest, Name, structure(Name, Attrs)).

build_structure([Key=[]|Rest], Name, structure(Name, [Key=listof(any)|Attrs])) :-
    build_structure(Rest, Name, structure(Name, Attrs)).

build_structure([Key=Val|Rest], Name, structure(Name, [Key=literal(Type)|Attrs])) :-
    Val \= json(_),
    not(is_list(Val)),
    type_of(Val, Type),
    build_structure(Rest, Name, structure(Name, Attrs)).

