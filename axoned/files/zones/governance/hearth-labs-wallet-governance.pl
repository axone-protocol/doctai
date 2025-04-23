% Hearth Labs Zone Governance
% ===========================

:- discontiguous([permitted_action/2, title/2, partOf/2, chapter/1, section/1, article/1, paragraph/2]).

% Metadata
chapter('chap1').
title('chap1', 'Hearth Labs Governance').

% Static rules
% ------------------

% Only this DID can update the zone
permitted_action('did:key:zQ3shN45CkRnjackWYwM191q2bNanAYeP1ZvxoAEPaV4ZpgZo', 'zone:update').

% Contributors that can register resources
contributor_id('did:key:z6DtrNpHS569CKDUFaXGzAMnLdMoaanxHqqsPnd91ZnyufLL').

permitted_action(DID, 'resource:register') :- contributor_id(DID).

% Query predicates
tell_permitted_actions(DID, Actions) :-
    findall(Action, permitted_action(DID, Action), Actions).

tell(DID, Action, permitted, 'default') :-
    permitted_action(DID, Action), !.
tell(_, _, denied, 'default').
