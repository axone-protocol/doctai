% MinIO S3 Governance
% ===========================

:- discontiguous([title/2, partOf/2, chapter/1, section/1, subSection/1, article/1, paragraph/2]).

% Structure
title('1', 'MinIO Service').
chapter('1').

    title('1.1', 'Service Access Control'). partOf('1.1', '1').
    section('1.1').

        title('1.1.1', 'Public Access'). partOf('1.1.1', '1.1').
        subSection('1.1.1').

            title('1.1.1.1', 'Read'). partOf('1.1.1.1', '1.1.1').
            article('1.1.1.1').

                title('1.1.1.1.1', 'Read is allowed for everyone'). partOf('1.1.1.1.1', '1.1.1.1').
                paragraph('1.1.1.1.1', permitted).

        title('1.1.2', 'Restricted Access'). partOf('1.1.2', '1.1').
        subSection('1.1.2').

            title('1.1.2.1', 'Store'). partOf('1.1.2.1', '1.1.2').
            article('1.1.2.1').

                title('1.1.2.1.1', 'Store is allowed for everyone'). partOf('1.1.2.1.1', '1.1.2.1').
                paragraph('1.1.2.1.1', permitted).

            title('1.1.2.2', 'Validate File Types'). partOf('1.1.2.2', '1.1.2').
            article('1.1.2.2').

                paragraph('1.1.2.2.1', permitted).
                paragraph('1.1.2.2.2', permitted).

            title('1.1.2.3', 'Validate File Sizes'). partOf('1.1.2.3', '1.1.2').
            article('1.1.2.3').

                title('1.1.2.3.1', 'Maximum file size is 50MB'). partOf('1.1.2.3.1', '1.1.2.3').
                paragraph('1.1.2.3.1', permitted).

        title('1.1.3', 'Governance'). partOf('1.1.3', '1.1').
        subSection('1.1.3').

            title('1.1.3.1', 'Change'). partOf('1.1.3.1', '1.1.3').
            article('1.1.3.1').

                paragraph('1.1.3.1.1', permitted).

% File size helpers
max_file_size('52428800').

trim_leading_zeros(S, T) :-
    atom_chars(S, Cs),
    drop_zeros(Cs, Clean),
    ( Clean = [] -> T = '0' ; atom_chars(T, Clean) ).

drop_zeros(['0'|T], R) :- drop_zeros(T, R).
drop_zeros(L, L).

pad_left_zeros(S, Padded) :-
    atom_length(S, Len),
    Missing is 8 - Len,
    Missing >= 0,
    length(Zeros, Missing),
    maplist(=('0'), Zeros),
    atom_chars(ZeroAtom, Zeros),
    atom_concat(ZeroAtom, S, Padded).

% Core rules
tell(_, 'read', permitted, ['1.1.1.1.1':permitted]).
tell(_, 'store', permitted, ['1.1.2.1.1':permitted]).
tell(_, 'validate:file:text/csv', permitted, ['1.1.2.2.1':permitted]).
tell(_, 'validate:file:application/json', permitted, ['1.1.2.2.2':permitted]).
tell(_, 'governance:change', permitted, ['1.1.3.1.1':permitted]).

% Dynamic rule with safety
tell(_, Action, permitted, ['1.1.2.3.1':permitted]) :-
    nonvar(Action),
    atom_concat('validate:file:size:', SizeAtomRaw, Action),
    trim_leading_zeros(SizeAtomRaw, SizeAtom),
    pad_left_zeros(SizeAtom, PaddedSize),
    max_file_size(Max),
    PaddedSize @=< Max.

% Default fallback
tell(_, _, prohibited, []).

tell_all(Who, Actions) :-
    bagof(Action:Result:Evidence, tell(Who, Action, Result, Evidence), Actions).

tell_permitted_actions(Who, PermittedActions) :-
    tell_all(Who, Actions),
    findall(PermittedAction, member(PermittedAction:permitted:_, Actions), PermittedActions).

