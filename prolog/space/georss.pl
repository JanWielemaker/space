/*  Part of SWI-Prolog

    Author:        Willem Robert van Hage
    E-mail:        W.R.van.Hage@vu.nl
    WWW:           http://www.few.vu.nl/~wrvhage
    Copyright (c)  2009-2012, Vrije Universiteit Amsterdam
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

:- module(georss,
	  [ georss_candidate/2,
	    georss_candidate/3,
	    georss_simple_candidate/2,
	    georss_gml_candidate/2,
	    georss_uri_shape_triple/5
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(dcg/basics)).
:- use_module(gml).

:- rdf_meta(georss_simple_candidate(r,?,?)).

:- rdf_register_ns(georss,'http://www.georss.org/georss/').
:- rdf_register_ns(foaf,'http://xmlns.com/foaf/0.1/').

%%	georss_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for RDF triples that
%	link URI to a Shape with GeoRSS RDF properties
%	(e.g. georss:where, georss:line, georss:polygon).
%	Both GeoRSS Simple and GML are supported.

georss_candidate(URI, Shape) :-
	georss_simple_candidate(URI, Shape).
georss_candidate(URI, Shape) :-
	georss_gml_candidate(URI, Shape).

%%	georss_candidate(?URI,?Shape,+Source) is nondet.
%
%	Finds URI-Shape pairs using georss_candidate/2 in RDF
%	that was loaded from a certain Source.

georss_candidate(URI, Shape, Source) :-
	var(Source),
	georss_simple_candidate(URI, Shape, Source).
georss_candidate(URI, Shape, Source) :-
	var(Source),
	georss_gml_candidate(URI, Shape, Source).

georss_candidate(URI, Shape, Source) :-
	nonvar(Source),
	georss_simple_candidate(URI, Shape, Source).
georss_candidate(URI, Shape, Source) :-
	nonvar(Source),
	georss_gml_candidate(URI, Shape, Source).

%
% GeoRSS Simple
%

%%	georss_simple_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for GeoRSS Simple properties
%	(e.g. georss:point, georss:line, georss:polygon) in the RDF database.

georss_simple_candidate(URI, Shape) :-
	georss_simple_candidate(URI, Shape, _).

georss_simple_candidate(URI, Point, Source) :-
	rdf(URI, georss:point, PointStringLit, Source),
	parse_poslist_literal(PointStringLit,[Point]).
georss_simple_candidate(URI, linestring(Line), Source) :-
	rdf(URI, georss:line, LineStringLit, Source),
	parse_poslist_literal(LineStringLit,Line).
georss_simple_candidate(URI, polygon([Line]), Source) :-
	rdf(URI, georss:polygon, LineStringLit, Source),
	parse_poslist_literal(LineStringLit,Line).
georss_simple_candidate(URI, box(Line), Source) :-
	rdf(URI, georss:box, LineStringLit, Source),
	parse_poslist_literal(LineStringLit,Line).
georss_simple_candidate(URI, Circle, Source) :-
	rdf(URI, georss:circle, CenterRadiusLit, Source),
	parse_circle_literal(CenterRadiusLit,Circle).

parse_poslist_literal(literal(Lit), Shape) :-
	atom_codes(Lit,LSC),
	phrase(poslist(Shape),LSC).

parse_circle_literal(literal(Lit), Shape) :-
	atom_codes(Lit,CRC),
	phrase(circle(Shape),CRC).

circle(circle(Center,Radius)) -->
	blanks, pos(Center), blank, blanks, float(Radius), blanks.

%%	georss_uri_shape_triple(+URI,+Shape,-Subject,-Predicate,-Object) is det.
%%	georss_uri_shape_triple(-URI,-Shape,+Subject,+Predicate,+Object) is det.
%
%	Converts between a URI-Shape pair and its GeoRSS simple RDF triple form.

georss_uri_shape_triple(URI, Shape, URI, P, O) :-
	(   (   var(URI)
	    ;	var(Shape)
	    )
	->  georss_simple_candidate(URI,Shape)
	;   true
	),
	functor(Shape,GeomType,_),
	georss_simple_predicate(GeomType, P),
	georss_simple_literal(Shape, O).

georss_simple_predicate(point,P) :- rdf_equal(georss:point,P).
georss_simple_predicate(linestring,P) :- rdf_equal(georss:line,P).
georss_simple_predicate(linearring,P) :- rdf_equal(georss:line,P).
georss_simple_predicate(polygon,P) :- rdf_equal(georss:polygon,P).

georss_simple_literal(X, literal(L)) :-
	phrase(coordinates(X), Atomics),
	atomic_list_concat(Atomics, ' ', L).

coordinates(polygon([Coords|_])) --> !, point_list(Coords).
coordinates(linestring(Coords)) --> !, point_list(Coords).
coordinates(linearing(Coords)) --> !, point_list(Coords).
coordinates(Point) --> point(Point).

point_list([]) --> [].
point_list([H|T]) -->
	point(H), point_list(T).

point(Point) -->
	{ Point =.. [ point | List ] },
	List.

%
% GeoRSS GML
%

%%	georss_gml_candidate(?URI,?Shape) is nondet.
%
%	Finds URI-Shape pairs by searching for GeoRSS GML properties
%	(i.e. georss:where) in the RDF database.
%	Uses gml_shape/2 to parse the XMLLiteral representing the GML shape.

georss_gml_candidate(URI, Shape) :-
	georss_gml_candidate(URI, Shape, _).

georss_gml_candidate(URI, Shape, Source) :-
        (   rdf_equal(georss:where, P)
        ;   rdf_equal(foaf:based_near, P)
        ),
        georss_gml_triple(URI, P, GML, Source),
	gml_shape(GML, Shape).

georss_gml_triple(URI, Property, GML, Source) :-
	rdf(URI, Property, Lit, Source),
	gml_literal(Lit, GML).

gml_literal(literal(type(_,GML)),GML) :- !.
gml_literal(literal(GML),GML).

poslist(T) --> blank_star, poslist_plus(T), blank_star, !.
poslist_plus([H|T]) --> pos(H), poslist_star(T).
poslist_star(T) --> blank_plus, poslist(T).
poslist_star([]) --> [], !.

pos(point(X,Y)) --> c(X), blank_plus, c(Y).
pos(point(X,Y,Z)) --> c(X), blank_plus, c(Y), blank_plus, c(Z).
pos(point(X,Y,Z,M)) --> c(X), blank_plus, c(Y), blank_plus, c(Z), blank_plus, c(M).
c(X) --> float(X).

blank_plus --> blank, blank_star, !.
blank_plus --> " ", !.
blank_star --> blanks, !.
blank_star --> [], !.
