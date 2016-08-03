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

:- module(dbpedia,
	  [  dbpedia_candidate/2,
	     dbpedia_candidate/3
	  ]).

:- use_module(library(semweb/rdf_db)).
:- rdf_register_ns(dbp, 'http://dbpedia.org/property/').

:- rdf_assert(dbp:'co_percent_e3_percent_b6rdinatenProperty',rdfs:subPropertyOf,dbp:coordinateProperty).

%%	dbpedia_candidate(?URI,?Point) is nondet.
%
%	Finds URI-Shape pairs of RDF resource that are place-tagged with
%	DBpedia's coordinatenProperty notation that capture
%	WGS84 latitude/longitude positions.

dbpedia_candidate(URI,Point) :-
	dbpedia_candidate(URI,Point,_).

%%	dbpedia_candidate(?URI,?Point,?Source) is nondet.
%
%	Finds URI-Shape pairs of RDF resource that are place-tagged with
%	DBpedia's coordinatenProperty notation that capture
%	WGS84 latitude/longitude positions.
%	From RDF that was loaded from a certain Source.

dbpedia_candidate(URI,point(Lat,Long),Source) :-
	rdf_has(URI,dbp:coordinateProperty,literal(lang(nl,Atom)),RealPred),
	(   var(Source)
	->  true
	;   rdf(URI, RealPred, literal(lang(nl,Atom)), Source)
	),
	sub_atom(Atom, B, _, _, '_type:'), !,
	sub_atom(Atom, 0, B, _, Before),
	atom_codes(Before, Coords),
	phrase(split([LatDeg,LatMin,LatSec,NS,
		      LongDeg,LongMin,LongSec,EW]),Coords),
	Lat is NS * (LatDeg + (LatMin / 60) + (LatSec / 3600)),
	Long is EW * (LongDeg + (LongMin / 60) + (LongSec / 3600)).

split([LatDeg,LatMin,LatSec,NS,
       LongDeg,LongMin,LongSec,EW]) -->
	num(LatDeg), "_", num(LatMin), "_", num(LatSec), "_", ns(NS), "_",
	num(LongDeg), "_", num(LongMin), "_", num(LongSec), "_", ew(EW).

ns(-1.0) --> "S".
ns(1.0) --> "N".
ns(-1.0) --> "Z".
ew(-1.0) --> "W".
ew(1.0) --> "E".
ew(1.0) --> "O".

num(Num) -->
	digits(Ds),
	{ (   Ds = []
	  ->  fail
	  ;   atomic_list_concat(Ds,Atom),
	      atom_number(Atom,Num)
	  )
	}.

digit(D) -->
	[C],
	{ code_type(C, digit(D)) }.

digits([D|Ds]) --> digit(D), digits(Ds).
digits([]) --> [].






