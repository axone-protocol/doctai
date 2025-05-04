% Minio S3 Governance
% ===========================

:- discontiguous([title/2, partOf/2, chapter/1, section/1, subSection/1, article/1, paragraph/4]).

% Main structure
title('1', 'MinIO Service').
chapter('1').

    title('1.1', 'Service Access Control'). partOf('1.1', '1').
    section('1.1').

        title('1.1.1', 'Public Access'). partOf('1.1.1', '1.1').
        subSection('1.1.1').

            title('1.1.1.1', 'Use'). partOf('1.1.1.1', '1.1.1').
            article('1.1.1.1').

                title('1.1.1.1.1', 'Use is allowed for everyone'). partOf('1.1.1.1.1', '1.1.1.1').
                paragraph('1.1.1.1.1', permitted, _, 'use').

        title('1.1.2', 'Restricted Access'). partOf('1.1.2', '1.1').
        subSection('1.1.2').

            title('1.1.2.1', 'Store'). partOf('1.1.2.1', '1.1.2').
            article('1.1.2.1').

                title('1.1.2.1.1', 'Store is allowed for everyone'). partOf('1.1.2.1.1', '1.1.2.1').
                paragraph('1.1.2.1.1', permitted, _, 'store').

            title('1.1.2.2', 'Read'). partOf('1.1.2.2', '1.1.2').
            article('1.1.2.2').

                title('1.1.2.2.1', 'Read is allowed for everyone'). partOf('1.1.2.2.1', '1.1.2.2').
                paragraph('1.1.2.2.1', permitted, _, 'read').

        title('1.1.3', 'Governance'). partOf('1.1.3', '1.1').
        subSection('1.1.3').

            title('1.1.3.1', 'Change'). partOf('1.1.3.1', '1.1.3').
            article('1.1.3.1').

                title('1.1.3.1.1', 'Governance change is allowed for everyone'). partOf('1.1.3.1.1', '1.1.3.1').
                paragraph('1.1.3.1.1', permitted, _, 'governance:change').

% Action resolution
resolved_action_context(Who, _, Context) :- Context = Who.

% SDK required predicates
tell(Who, Action, Result, Evidence) :-
    resolved_action_context(Who, Action, Context),
    bagof(P:Modality, paragraph(P, Modality, Context, Action), Evidence),
    (   member(_: permitted, Evidence) -> Result = permitted
    ;   Result = prohibited
    ).

tell_all(Who, Actions) :-
    bagof(Action:Result:Evidence, tell(Who, Action, Result, Evidence), Actions).

tell_permitted_actions(Who, PermittedActions) :-
    tell_all(Who, Actions),
    findall(PermittedAction, member(PermittedAction:permitted:_, Actions), PermittedActions).
