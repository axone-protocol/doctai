# Trying out some queries

```bash

$AXONED_PATH query logic ask \
--node $AXONE_NODE_RPC \
--program "memberOf(alice, council). memberOf(bob,council). can_vote(X) :- memberOf(X, council)." \
"can_vote(X)."

$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "max_file_size(['5','2','4','2','8','8','0','0']).
is_below_max(S) :- max_file_size(Max), S = ['5','2','4','2','8','8','0','0'], S @=< Max." \
"is_below_max(['1','0','0','0'])."

$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "max_file_size(['5','2','4','2','8','8','0','0']).
less_than_digits([], [_|_]).
less_than_digits([H|T1], [H|T2]) :- less_than_digits(T1, T2).
less_than_digits([H1|_], [H2|_]) :- char_code(H1, C1), char_code(H2, C2), C1 < C2.
is_smaller(A, B) :- length(A, LA), length(B, LB), ( LA < LB -> true ; (LA =:= LB, less_than_digits(A, B)) ).
is_below_max(S) :- max_file_size(Max), is_smaller(S, Max)." \
"is_below_max(['5','0','0','0','0','0'])."

$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "max_file_size(['5','2','4','2','8','8','0','0']).
less_than_digits([], [_|_]).
less_than_digits([H1|T1], [H2|T2]) :- char_code(H1, C1), char_code(H2, C2), ( C1 < C2 -> true ; C1 =:= C2 -> less_than_digits(T1, T2) ).
is_smaller_chars(AAtom, BAtom) :- atom_chars(AAtom, AList), atom_chars(BAtom, BList), length(AList, LA), length(BList, LB), ( LA < LB -> true ; LA =:= LB -> less_than_digits(AList, BList) ).
is_below_max(SAtom) :- max_file_size(MaxList), atom_chars(MaxAtom, MaxList), is_smaller_chars(SAtom, MaxAtom)." \
"is_below_max('5000000')."


$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "tell(_, Action, Result, Evidence) :- \
  atom_concat('validate:file:size:', Size, Action), \
  Size @=< '52428800' -> \
    Evidence = ['1.1.2.3.1':permitted], \
    Result = permitted. \
tell(_, 'read', permitted, ['1.1.1.1.1':permitted])." \
"tell(_, 'validate:file:size:052428800', R, E)."


$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "tell(_, Action, Result, Evidence) :- \
  atom_concat('validate:file:size:', Size, Action), \
  Size @=< '52428800' -> \
    Evidence = ['1.1.2.3.1':permitted], \
    Result = permitted. \
tell(_, 'read', permitted, ['1.1.1.1.1':permitted])." \
"tell(_, 'validate:file:size:005000000', R, E)."

$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "tell(_, Action, Result, Evidence) :- \
  atom_concat('validate:file:size:', Size, Action), \
  Size @=< '52428800' -> \
    Evidence = ['1.1.2.3.1':permitted], \
    Result = permitted. \
tell(_, 'read', permitted, ['1.1.1.1.1':permitted])." \
"tell(_, 'validate:file:size:999999999', R, E)."


$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "pad_left_zeros(S, Padded) :- \
  atom_length(S, Len), \
  Missing is 8 - Len, \
  Missing >= 0, \
  length(Zeros, Missing), \
  maplist(=('0'), Zeros), \
  atom_chars(ZeroAtom, Zeros), \
  atom_concat(ZeroAtom, S, Padded)." \
"pad_left_zeros('123', P)."




$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "max_file_size('52428800').

pad_left_zeros(S, Padded) :- \
  atom_length(S, Len), \
  Missing is 8 - Len, \
  Missing >= 0, \
  length(Zeros, Missing), \
  maplist(=('0'), Zeros), \
  atom_chars(ZeroAtom, Zeros), \
  atom_concat(ZeroAtom, S, Padded).

tell(_, Action, Result, Evidence) :- \
  atom_concat('validate:file:size:', SizeAtom, Action), \
  pad_left_zeros(SizeAtom, PaddedSize), \
  max_file_size(Max), \
  PaddedSize @=< Max -> \
    Evidence = ['1.1.2.3.1':permitted], \
    Result = permitted.

tell(_, 'read', permitted, ['1.1.1.1.1':permitted])." \
"tell(_, 'validate:file:size:0052000000', R, E)."

$AXONED_PATH query logic ask \
--node "$AXONE_NODE_RPC" \
--program "max_file_size('52428800').

trim_leading_zeros(S, T) :- \
  atom_chars(S, Cs), \
  drop_zeros(Cs, Clean), \
  ( Clean = [] -> T = '0' ; atom_chars(T, Clean) ).

drop_zeros(['0'|T], R) :- drop_zeros(T, R).
drop_zeros(L, L).

pad_left_zeros(S, Padded) :- \
  atom_length(S, Len), \
  Missing is 8 - Len, \
  Missing >= 0, \
  length(Zeros, Missing), \
  maplist(=('0'), Zeros), \
  atom_chars(ZeroAtom, Zeros), \
  atom_concat(ZeroAtom, S, Padded).

tell(_, Action, Result, Evidence) :- \
  atom_concat('validate:file:size:', SizeAtomRaw, Action), \
  trim_leading_zeros(SizeAtomRaw, SizeAtom), \
  pad_left_zeros(SizeAtom, PaddedSize), \
  max_file_size(Max), \
  PaddedSize @=< Max -> \
    Evidence = ['1.1.2.3.1':permitted], \
    Result = permitted.

tell(_, 'read', permitted, ['1.1.1.1.1':permitted])." \
"tell(_, 'validate:file:size:0054000000', R, E)."

```
