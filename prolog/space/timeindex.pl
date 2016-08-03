/*  Part of SWI-Prolog

    Author:        Willem Robert van Hage
    E-mail:        W.R.van.Hage@vu.nl
    WWW:           http://www.few.vu.nl/~wrvhage
    Copyright (c)  2010-2013, Vrije Universiteit Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*
    Internally, all time objects are stored as intervals.
    point(T) is translated to interval(T,T).
    If fuzzy time intervals,
    interval(EarlyBegin,LateBegin,EarlyEnd,LateEnd),
    should ever be implemented, this could be done by postprocessing
    over operations on an index containing interval(EarlyBegin,LateEnd).

    Per handle there are two indices, one containing the begin points
    and the other containing the end points of the intervals.
    The index containing the end points actually contains negative time
    points to inverse the order of the index.
*/

% TODO: Make version of atom_map that allows double type datums and
%	that has nondet search functions.
%	This would remove the need for the explicit EpochOffset and
%	would speed up all search predicates.

:- module(timeindex,
	  [  time_index/1,        % ?Index
	     time_index/4,        % +Index, ?BeginIndex, ?EndIndex, ?Epoch
	     time_setting/2,	  % +Index ?Setting
	     time_setting/1,	  % ?Setting  (uses default index)
	     time_assert/3,       % +URI, +Time, +Index
	     time_assert/2,	  % +URI, +Time  (uses default index)
	     time_retract/3,      % +URI, +Time, +Index
	     time_retract/2,      % +URI, +Time  (uses default index)
	     time_clear/2,	  % +Index, +NewEpochOffset
	     time_clear/1,	  % +Index
	     time_clear/0,        % (uses default index)
	     time_index_all/1,    % +Index
	     time_index_all/0,    % (uses default index)
	     time_bulkload/2,     % :CandidatePred, +Index
	     time_bulkload/1,     % :CandidatePred
	     time_intersects/3,	  % +Time, -URI, +Index
	     time_intersects/2,	  % (uses default index)
	     time_contains/3,	  % +Time, -URI, +Index
	     time_contains/2,	  % (uses default index)
	     time_prev_end/3,	  % +Time, -URI, +Index
	     time_prev_end/2,     % (uses default index)
	     time_next_begin/3,   % +Time, -URI, +Index
	     time_next_begin/2,   % (uses default index)
	     uri_time/4,          % ?URI, ?Time, ?Source, +EpochOffset
	     uri_time/3,          % ?URI, ?Time, ?Source (uses offset 0)
	     uri_time/2,          % ?URI, ?Time (uses offset 0)
	     parse_timestamp/3,   % +TimeStamp, -Epoch, +EpochOffset
	     parse_timestamp/2,   % ?TimeStamp, ?Epoch (uses offset 0)

             % Allen's Interval Algebra
	     time_before/2,       % +Time, +Time
	     time_after/2,
	     time_meets/2,
	     time_meets_inverse/2,
	     time_overlaps/2,
	     time_overlaps_inverse/2,
	     time_starts/2,
	     time_starts_inverse/2,
	     time_during/2,
	     time_during_inverse/2,
	     time_finishes/2,
	     time_finishes_inverse/2,
	     time_equal/2,

	     % Time operations
	     time_expand_before/3,   % +Time, +Duration, -Time
	     time_expand_after/3,    % +Time, +Duration, -Time
	     time_expand/3,	     % +Time, +Duration, -Time
	     time_duration/2,        % +Time, -Duration
	     time_duration_before/3, % +Time, +Duration, -Time
	     time_duration_after/3,  % +Time, +Duration, -Time
	     time_duration/3,        % +Time, +Time, -Duration (epoch)
	     time_between/3,         % +Time, +Time, -Duration (epoch)

	     timestamp_duration/2   % +TimeStamp, -Duration (duration(Y,M,D,H,Min,S))
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

:- dynamic time_indices/4.

:- rdf_meta  time_index(r),
	     time_index(r,?,?,?),
	     time_setting(r,?),
	     time_assert(r,?,r),
	     time_assert(r,?),
	     time_retract(r,?,r),
	     time_retract(r,?),
	     time_clear(r,?),
	     time_clear(r),
	     time_index_all(r),
	     time_bulkload(?,r),
	     time_intersects(?,r,r),
	     time_intersects(?,r),
	     time_contains(?,r,r),
	     time_contains(?,r),
	     time_prev_end(?,r,r),
	     time_prev_end(?,r),
	     time_next_begin(?,r,r),
	     time_next_begin(?,r),
	     uri_time(r,?,t,?),
	     uri_time(r,?,t),
	     uri_time(r,?).


time_index(Index) :- time_indices(Index,_,_,_).

time_index(Index, IdxB, IdxE, Epoch) :-
	time_indices(Index, IdxB, IdxE, Epoch), !.
time_index(Index, IdxB, IdxE, Epoch) :-
	nonvar(Epoch),
	time_new(Index, Epoch),
	time_indices(Index, IdxB, IdxE, Epoch), !.
time_index(Index, IdxB, IdxE, Epoch) :-
	var(Epoch),
%	time_new(Index),
	time_indices(Index, IdxB, IdxE, Epoch), !.

%%	time_setting(?Option) is det.
%
%	Sets/retrieves settings and current values of an index.
%	Supported Options are:
%	size(-N), N is the number of URI-Time pairs in the index.
%	epoch(+Epoch), sets a new Epoch for the index, clears the index.
%       epoch(-Epoch), Epoch is the current Epoch of the index.
%
time_setting(Option) :- time_setting(default, Option).
time_setting(Index, size(N)) :-
	time_indices(Index,B,_,_), !,
	rdf_statistics_literal_map(B,size(N,_)).
time_setting(Index, epoch(E)) :-
	var(E),
	time_indices(Index,_,_,E), !.
time_setting(Index, epoch(E)) :-
	nonvar(E),
	format('% Clearing index ~w, setting new Epoch to ~w\n', [Index,E]),
	time_clear(Index, E).


%%	time_new(+IndexName) is det.
%%	time_new(+IndexName,+Offset) is det.
%
%	Creates a new temporal index.
%       Offset is the epoch used internally by the index.
%
time_new(Index) :- time_new(Index, 0).
time_new(Index, EpochOffset) :-
	rdf_new_literal_map(B),
	rdf_new_literal_map(E),
	assert(time_indices(Index, B, E, EpochOffset)).


%%	time_assert(+URI,+Time) is det.
%%	time_assert(+URI,+Time,+IndexName) is det.
%
%	Inserts a new URI-Time pair into the index.
%
time_assert(URI, T) :- time_assert(URI, T, default).
time_assert(URI, interval(TB, TE), Index) :-
	(   time_index(Index)
	->  true
	;   time_clear(Index, TB)
	),
	time_index(Index, IdxB, IdxE, EpochOffset),
	TBE is integer(TB - EpochOffset),
	TEE is integer(-1 * (TE - EpochOffset)),
	rdf_insert_literal_map(IdxB, TBE, URI),
	rdf_insert_literal_map(IdxE, TEE, URI), !.
time_assert(URI, point(T), Index) :-
	time_assert(URI, interval(T,T), Index), !.
time_assert(URI, TimeExpr, Index) :-
	atom(TimeExpr),
	parse_timestamp(TimeExpr, T),
	time_assert(URI, point(T), Index), !.
time_assert(URI, TimeExpr, Index) :-
	number(TimeExpr),
	time_assert(URI, point(TimeExpr), Index), !.

%%	time_retract(+URI,+Time) is det.
%%	time_retract(+URI,+Time,+IndexName) is det.
%
%	Removes a URI-Time pair from the index.
%
time_retract(URI, T) :- time_retract(URI, T, default).
time_retract(URI, interval(TB, TE), Index) :-
	time_indices(Index, IdxB, IdxE, EpochOffset),
	TBE is integer(TB - EpochOffset),
	TEE is integer(-1 * (TE - EpochOffset)),
	rdf_delete_literal_map(IdxB, TBE, URI),
	rdf_delete_literal_map(IdxE, TEE, URI).

%%	time_clear(+IndexName,+NewOffset) is det.
%%	time_clear(+IndexName) is det.
%%	time_clear is det.
%
%	Clears an index. Optionally sets a new epoch for the index
%	that will be used for all future asserts into the index.
%
time_clear :- time_clear(default).
time_clear(Index) :-
	(   time_index(Index, IdxB, IdxE, OldEpochOffset)
	->  retractall(time_indices(Index, _, _, _)),
	    rdf_destroy_literal_map(IdxB),
	    rdf_destroy_literal_map(IdxE),
	    time_new(Index, OldEpochOffset)
	;   true
	).
time_clear(Index, NewEpochOffset) :-
	number(NewEpochOffset),
	(   time_index(Index, IdxB, IdxE, _OldEpochOffset)
	->  retractall(time_indices(Index, _, _, _)),
	    rdf_destroy_literal_map(IdxB),
	    rdf_destroy_literal_map(IdxE)
	;   true
	),
	time_new(Index, NewEpochOffset), !.

%%	time_index_all(+IndexName) is det.
%%	time_index_all is det.
%
%	Adds all URI-Time pairs found by the uri_time predicate
%	into the index.
%
time_index_all :- time_index_all(default).
time_index_all(Index) :- time_bulkload(uri_time, Index).

:- meta_predicate time_bulkload(2), time_bulkload(2,+).

%%	time_bulkload(:CandidatePred,+Index) is det.
%%	time_bulkload(:CandidatePred) is det.
%
%	Like time_index_all, but indexes URI-Time pairs found by the
%	custom predicate CandidatePred.
%
time_bulkload(CandidatePred) :- time_bulkload(CandidatePred, default).
time_bulkload(CandidatePred, Index) :-
	time_clear(Index),
	forall(call(CandidatePred, URI, Time),
	       time_assert(URI, Time, Index)),
	time_index(Index,B,_,_),
	rdf_statistics_literal_map(B,size(K,_)),
	format('% Added ~w URI-Time pairs to ~w\n',[K,Index]).


%%	time_intersects(+Time,-URI,+Index) is nondet.
%%	time_intersects(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that intersects with the interval Time = interval(Begin,End).
%
%       NB! The implementation currently does not return intervals
%       that contain the query interval, hence the name time_intersects
%       is currently a misnomer.
%
time_intersects(T, URI) :- time_intersects(T, URI, default).

time_intersects(point(T), URI, Index) :-
	time_intersects(interval(T,T), URI, Index).
time_intersects(interval(TB, TE), URI, Index) :-
	time_intersects_impl1(interval(TB, TE), URI, Index).
% FIXME: buggy implementation, does not find intervals that start
% before the query interval and end after the query interval.
% The obvious solution would be to compute the intersection of the
% set of intervals ending after the begin of the query and the
% set of intervals starting before the end of the query, but that
% is a very expensive query.
% As soon as there is a nondet version of rdf_keys_in_literal_map
% this implementation could become viable if the hard solutions are
% delayed until after all easy solutions have been found.
time_intersects_impl1(interval(TB, TE), URI, Index) :-
	time_index(Index, IdxB, IdxE, EO),
	parse_timestamp(TB, TBE, EO),
	parse_timestamp(TE, TEE, EO),
	TBI is integer(TBE),
	TEI is integer(TEE),
	rdf_keys_in_literal_map(IdxB, between(TBI, TEI), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, between(TBI, TEI), BeginOr),
	rdf_litindex:lookup(BeginOr, IdxB, B2, B3),
	match_results(B2, B3, B4),
	TBR is integer(-1 * TBE),
	TER is integer(-1 * TEE),
	rdf_keys_in_literal_map(IdxE, between(TER, TBR), EndMatch),
	rdf_litindex:list_to_or(EndMatch, between(TER, TBR), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_results(E2, E3, E4),
	append(E4, B4, Matches),
	% predsort(ord, E4B4, Matches), !,
	pairs_values(Matches, Values), !,
	list_to_set(Values, ValueSet),
	member(URI, ValueSet).

%%	time_contains(+Time,-URI,+Index) is nondet.
%%	time_contains(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that are contained by the interval Time = interval(Begin,End).
%
time_contains(T, URI) :- time_contains(T, URI, default).
time_contains(interval(-,-), URI, Index) :- !,
	time_contains_all(interval(-,-), URI, Index).
time_contains(interval(-,End), URI, Index) :- !,
	time_contains_le(interval(-,End), URI, Index).
time_contains(interval(Begin,-), URI, Index) :- !,
	time_contains_ge(interval(Begin,-), URI, Index).
time_contains(interval(TB, TE), URI, Index) :-
	time_index(Index, IdxB, IdxE, EO),
	parse_timestamp(TB, TBE, EO),
	TBI is integer(TBE),
	TBR is integer(-1 * TBE),
	parse_timestamp(TE, TEE, EO),
	TEI is integer(TEE),
	TER is integer(-1 * TEE),
	rdf_keys_in_literal_map(IdxB, between(TBI, TEI), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, between(TBI, TEI), BeginOr),
	rdf_litindex:lookup(BeginOr, IdxB, B2, B3),
	match_results(B2, B3, B4),
	rdf_keys_in_literal_map(IdxE, between(TER, TBR), EndMatch),
	rdf_litindex:list_to_or(EndMatch, between(TER, TBR), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_results(E2, E3, E4),
	predsort(ord, B4, BS),
	predsort(rev, E4, ES),
	pairs_values(BS, BSValues),
	pairs_values(ES, ESValues),
	ord_intersection(BSValues, ESValues, Matches), !,
	member(URI, Matches).

lookup(Index,Key,A,B) :-
       rdf_litindex:lookup(Key, Index, [A], [B]).

time_contains_all(interval(-,-), URI, Index) :-
	time_index(Index, IdxB, IdxE, _EO),
	rdf_keys_in_literal_map(IdxB, all, BeginMatch),
	rdf_keys_in_literal_map(IdxE, all, EndMatch),
	maplist(lookup(IdxB),BeginMatch,BM1,BM2),
	maplist(lookup(IdxB),EndMatch,EM1,EM2),
	match_results(BM1, BM2, BMatches),
	match_results(EM1, EM2, EMatches),
	pairs_values(BMatches, BSValues),
	pairs_values(EMatches, ESValues),
	append(BSValues, ESValues, Matches2),
	list_to_set(Matches2,Matches), !,
	member(URI, Matches).
time_contains_le(interval(-, TE), URI, Index) :-
	time_prev_end(point(TE), URI, Index).
time_contains_ge(interval(TB, -), URI, Index) :-
	time_next_begin(point(TB), URI, Index).

%%	time_prev_end(+Time,-URI,+Index) is nondet.
%%	time_prev_end(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that ends before time point or interval Time in
%	order of increasing duration.
%
time_prev_end(point(T), URI) :- time_prev_end(interval(T,T), URI, default).
time_prev_end(interval(T,T1), URI) :- time_prev_end(interval(T,T1), URI, default).
time_prev_end(point(T), URI, Index) :- time_prev_end(interval(T,T), URI, Index).
time_prev_end(interval(_,T), URI, Index) :-
	time_index(Index, _, IdxE, EO),
	parse_timestamp(T, TE, EO),
	TER is integer(-1 * TE),
	rdf_keys_in_literal_map(IdxE, ge(TER), EndMatch),
	rdf_litindex:list_to_or(EndMatch, ge(TER), EndOr),
	rdf_litindex:lookup(EndOr, IdxE, E2, E3),
	match_result(E2, E3, _-URI).

%%	time_next_begin(+Time,-URI,+Index) is nondet.
%%	time_next_begin(+Time,-URI) is nondet.
%
%	Finds all URIs that have an indexed time interval
%	that begins after time point or interval Time in order of
%	increasing duration.
%
time_next_begin(point(T), URI) :- time_next_begin(interval(T,T), URI, default).
time_next_begin(interval(T0,T), URI) :- time_next_begin(interval(T0,T), URI, default).
time_next_begin(point(T), URI, Index) :- time_next_begin(interval(T,T), URI, Index).
time_next_begin(interval(T,_), URI, Index) :-
	time_index(Index, IdxB, _, EO),
	parse_timestamp(T, TE, EO),
	TEI is integer(TE),
	rdf_keys_in_literal_map(IdxB, ge(TEI), BeginMatch),
	rdf_litindex:list_to_or(BeginMatch, ge(TEI), BeginOr),
	rdf_litindex:lookup(BeginOr, IdxB, B2, B3),
	match_result(B2, B3, _-URI).

zip_tree_member([H0|T0], [H1|T1], R) :-
	is_list(H0),
	(   zip_tree_member(H0, H1, R)
	;   zip_tree_member(T0, T1, R)
	).
zip_tree_member([[H0]], [H1|T1], R) :-
	(   member(H, H1),
	    R = H0-H
	;   zip_tree_member([H0], T1, R)
	).

zip_tree(_, [], []).
zip_tree(A, B, A-B) :- \+is_list(A).
zip_tree([], [B], nil-B).
zip_tree([H0], [H1|T1], [H|T]) :-
	\+is_list(H0),
	zip_tree(H0, H1, H),
	zip_tree([H0], T1, T).
zip_tree([H0|T0], [H1|T1], [H|T]) :-
	zip_tree(H0, H1, H),
	zip_tree(T0, T1, T).

match_results(A, B, C) :-
	zip_tree(A, B, Z),
	flatten(Z, C).
match_result(A, B, C) :-
	zip_tree_member(A, B, C).

ord(>, between(_,_,A)-_, between(_,_,B)-_) :- A > B.
ord(<, between(_,_,A)-_, between(_,_,B)-_) :- A < B.
ord(>, le(_,A)-_, le(_,B)-_) :- A > B.
ord(<, le(_,A)-_, le(_,B)-_) :- A < B.
ord(=, _, _).
rev(>, between(_,_,A)-_, between(_,_,B)-_) :- A < B.
rev(<, between(_,_,A)-_, between(_,_,B)-_) :- A > B.
rev(>, le(_,A)-_, le(_,B)-_) :- A < B.
rev(>, le(_,A)-_, le(_,B)-_) :- A > B.
rev(=, _, _).

%%	uri_time(?URI,?Time,?Source,+Offset) is semidet.
%%	uri_time(?URI,?Time,?Source) is semidet.
%%	uri_time(?URI,?Time) is semidet.
%
%	Finds all URI-Time pairs described in the RDF database.
%	Source matches the graphs in which the pair is described.
%	Optionally, the Begin and End of the time interval are
%	returned with respect to a given Offset.
%
uri_time(URI, interval(Begin, End)) :- uri_time(URI, interval(Begin, End), _Source, 0).
uri_time(URI, interval(Begin, End), Source) :- uri_time(URI, interval(Begin, End), Source, 0).
uri_time(URI, interval(Begin, End), Source, EpochOffset) :-
	time_candidate(URI, interval(TB,TE), Source),
	parse_timestamp(TB, Begin0),
	parse_timestamp(TE, End0),
	Begin is Begin0 - EpochOffset,
	End is End0 - EpochOffset.

:- rdf_register_ns(sem, 'http://semanticweb.cs.vu.nl/2009/11/sem/').
:- rdf_register_ns(owltime, 'http://www.w3.org/2006/time#').

time_candidate(URI, TimeStamp) :- time_candidate(URI, TimeStamp, _Source).
time_candidate(URI, TimeStamp, Source) :-
	owl_time_xsd_candidate(URI, TimeStamp, Source).
time_candidate(URI, TimeStamp, Source) :-
	sem_time_candidate(URI, TimeStamp, Source).

sem_time_candidate(URI, TimeStamp) :- sem_time_candidate(URI, TimeStamp, _Source).
sem_time_candidate(URI, interval(Begin,End), Source) :-
	nonvar(Source),
	(   rdf(URI, sem:hasBeginTimeStamp, literal(TB), Source)
	->  (   TB = type(_,Begin)
	    ->  true
	    ;   Begin = TB
	    )
	;   Begin = -
	),
        (   rdf(URI, sem:hasEndTimeStamp, literal(TE), Source)
	->  (   TE = type(_,End)
	    ->  true
	    ;   End = TE
	    )
	;   End = -
	),
	(   Begin = -, End = -
	->  fail
	;   true
	).
sem_time_candidate(URI, interval(Begin,End), Source) :-
	var(Source),
	(   rdf(URI, sem:hasBeginTimeStamp, literal(TB), Source1)
	->  (   TB = type(_,Begin)
	    ->  true
	    ;   Begin = TB
	    )
	;   Begin = -
	),
	(   Source1 = Source:_
	->  true
	;   Source = Source1
	),
        (   rdf(URI, sem:hasEndTimeStamp, literal(TE), Source)
	->  (   TE = type(_,End)
	    ->  true
	    ;   End = TE
	    )
	;   End = -
	),
	(   Begin = -, End = -
	->  fail
	;   true
	).

sem_time_candidate(URI, interval(TimeStamp,TimeStamp), Source) :-
	rdf(URI, sem:hasTimeStamp, literal(L), Source),
	(   L = type(_,TimeStamp)
	->  true
	;   L = TimeStamp
	).

owl_time_xsd_candidate(URI, interval(TimeStamp,TimeStamp), Source) :-
        rdf(URI, owltime:inXSDDateTime, literal(L), Source),
        (   L = type(_,TimeStamp)
        ->  true
        ;   L = TimeStamp
        ).

%%	parse_timestamp(?TimeStampAtom, ?EpochTimeStamp) is det.
%
%	Converts in both directions between a literal time
%	representation and a numerical time representation based on the
%	epoch.
%	The format of the generated atom is ISO 8601 in UTC.
%
parse_timestamp(TimeStamp, Epoch) :-
	var(TimeStamp),
	number(Epoch),
	stamp_date_time(Epoch, Date, 'UTC'),
	format_time(atom(TimeStamp), '%FT%TZ', Date), !.
parse_timestamp(TimeStamp, Epoch) :- parse_timestamp(TimeStamp, Epoch, 0).

%%	parse_timestamp(+TimeStampAtom, -Epoch, +EpochOffset) is det.
%
%	Converts between a literal TimeStamp atom and a numerical
%	time representation based on the epoch - EpochOffset.
%	This allows for a more fine grained representation of time
%	for time points far away from the epoch.
%
parse_timestamp(TimeStamp, Epoch, EpochOffset) :-
	nonvar(TimeStamp),
	(   number(TimeStamp)
	->  E = TimeStamp
	;   atom(TimeStamp), iso_timestamp_epoch(TimeStamp, E), !
	;   atom(TimeStamp), sic_timestamp_epoch(TimeStamp, E), !
	;   \+atom(TimeStamp), timex_timestamp_epoch(TimeStamp, E), !
	% extend here
	),
	Epoch is E - EpochOffset.


iso_timestamp_epoch(TimeStamp, T) :-
	parse_time(TimeStamp, T).

sic_timestamp_epoch(TimeStamp, T) :-
	atom_number(TimeStamp, T).

timex_timestamp_epoch(type(Type,TimeStamp), T) :-
	rdf_equal(Type, rdf:'XMLLiteral'),
	xml_timestamp(TimeStamp, T).

xml_timestamp(TimeStamp, T) :-
	(   xpath(TimeStamp, //(timex2), element(_,Attr,_)) % plain XML timex2 tag
	->  true
	;   xpath(TimeStamp, //(_:timex2), element(_,Attr,_)) % timex2 in some namespace
	), !,
	memberchk('VAL'=ISO, Attr),
	parse_time(ISO, T). % Doesn't deal with local time
xml_timestamp(TimeStamp, T) :-
	(   xpath(TimeStamp, //(timex3), element(_,Attr,_)) % plain XML timex3 tag
	->  true
	;   xpath(TimeStamp, //(_:timex3), element(_,Attr,_)) % timex3 in some namespace
	), !,
	memberchk(value=ISO, Attr),
	parse_time(ISO, T). % Doesn't deal with local time


/*
 *  Allen's interval algebra
 */

% assumes point(T) is equal to interval(T,T)

time_to_interval(interval(T1,T2), interval(T1,T2)) :-
	number(T1), number(T2), !.
time_to_interval(point(T), interval(T,T)) :-
	number(T), !.
time_to_interval(interval(T1,T2), interval(T3,T4)) :-
	parse_timestamp(T1,T3),
	parse_timestamp(T2,T4), !.
time_to_interval(point(T1), interval(T2,T2)) :-
	parse_timestamp(T1,T2), !.
time_to_interval(T1, interval(T2,T2)) :-
	parse_timestamp(T1,T2), !.


time_overlaps(Ta, Tb) :-
	time_to_interval(Ta,interval(T1,T2)),
	time_to_interval(Tb,interval(T3,T4)),
	T1 =< T4,
	T2 >= T3.

time_overlaps_inverse(T1,T2) :- time_overlaps(T2,T1).

time_during(Ta, Tb) :-
	time_to_interval(Ta, interval(T1,T2)),
	time_to_interval(Tb, interval(T3,T4)),
	T1 =< T3,
	T2 >= T4.

time_during_inverse(T1,T2) :- time_during(T2,T1).

time_before(Ta, Tb) :-
	time_to_interval(Ta, interval(_,T1)),
	time_to_interval(Tb, interval(T2,_)),
	T1 < T2.

time_after(T1,T2) :- time_before(T2,T1).

time_meets(Ta, Tb) :-
	time_to_interval(Ta, interval(_,T)),
	time_to_interval(Tb, interval(T,_)).

time_meets_inverse(T1,T2) :- time_meets_inverse(T2,T1).

time_starts(Ta, Tb) :-
	time_to_interval(Ta, interval(T,T1)),
	time_to_interval(Tb, interval(T,T2)),
	T1 =< T2.

time_starts_inverse(T1,T2) :- time_starts(T2,T1).

time_finishes(Ta, Tb) :-
	time_to_interval(Ta, interval(T1,T)),
	time_to_interval(Tb, interval(T2,T)),
	T1 >= T2.

time_finishes_inverse(T1,T2) :- time_finishes(T2,T1).

time_equal(T,T) :- time_to_interval(T,_).


/*
 *  Operations on Time points and intervals
 */

time_expand_before(T, Margin, interval(T1,Tb)) :-
	time_to_interval(T, interval(Ta,Tb)),
	T1 is Ta - Margin.

time_expand_after(T, Margin, interval(Ta,T1)) :-
	time_to_interval(T, interval(Ta,Tb)),
	T1 is Tb + Margin.

time_expand(T, Margin, interval(T1,T2)) :-
	time_to_interval(T, interval(Ta,Tb)),
	T1 is Ta - Margin,
	T2 is Tb + Margin.

time_duration(T, D) :-
	time_to_interval(T, interval(Ta,Tb)),
	D is Tb - Ta.

time_duration_before(T, D, point(T2)) :-
	time_to_interval(T,interval(T1,_)),
	T2 is T1 - D.

time_duration_after(T, D, point(T2)) :-
	time_to_interval(T,interval(_,T1)),
	T2 is T1 + D.

time_duration(Ta, Tb, D) :-
	time_to_interval(Ta, interval(T0,T1)),
	time_to_interval(Tb, interval(T2,T3)),
	(   time_before(interval(T0,T1), interval(T2,T3))
	->  time_duration(interval(T0,T3), D)
	;   time_duration(interval(T2,T1), D0),
	    D is -1 * D0
	).

time_between(Ta, Tb, D) :-
	time_to_interval(Ta, interval(T0,T1)),
	time_to_interval(Tb, interval(T2,T3)),
	(   time_before(interval(T0,T1), interval(T2,T3))
	->  time_duration(interval(T1,T2), D)
	;   time_duration(interval(T3,T0), D0),
	    D is -1 * D0
	).

timestamp_duration(TimeStamp, duration(Years,Months,Days,Hours,Minutes,Seconds)) :-
	abs(TimeStamp,T),
%	sign(TimeStamp,Sign),
	stamp_date_time(0, date(Y0,M0,D0,H0,Mi0,S0,_,_,_), 0),
	stamp_date_time(T, date(Y1,M1,D1,H1,Mi1,S1,_,_,_), 0),
	Years is Y1 - Y0,
	Months is M1 - M0,
	Days is D1 - D0,
	Hours is H1 - H0,
	Minutes is Mi1 - Mi0,
	Seconds is S1 - S0.

parse_duration(ISO, duration(Y,M,D,H,Mi,S)) :-
	phrase(duration(Y,M,D,H,Mi,S),Ts),
	atom_codes(ISO,Ts), !.


% does not follow the entire ISO 8601 spec yet
duration(Y,M,D,H,Mi,S) -->
	{ nonvar(Y),
	  number_codes(Y,Yc),
	  number_codes(M,Mc),
	  number_codes(D,Dc),
	  number_codes(H,Hc),
	  number_codes(Mi,Mic),
	  number_codes(S,Sc)
	},
	"P", Yc, "Y", Mc, "M", Dc, "D", "T", Hc, "H", Mic, "M", Sc, "S".

duration(Y,M,D,H,Mi,S) -->
	{ var(Y) },
	"P", Yc, "Y", Mc, "M", Dc, "D", "T", Hc, "H", Mic, "M", Sc, "S",
	{
	  number_codes(Y,Yc),
	  number_codes(M,Mc),
	  number_codes(D,Dc),
	  number_codes(H,Hc),
	  number_codes(Mi,Mic),
	  number_codes(S,Sc)
	}.





