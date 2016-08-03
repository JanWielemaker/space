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

:- module(gml,
	  [ gml_shape/2
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/basics)).
:- use_module(library(memfile)).
:- use_module(library(xpath)).
:- use_module(library(sgml)).
:- use_module(library(lists)).

%%	gml_shape(?GML,?Shape) is semidet.
%
%	Converts between the GML serialization of a shape and its
%	internal Prolog term representation.

gml_shape(GML, Geom) :-
        (   var(Geom)
	->  atom_to_memory_file(GML, Memfile),
	    open_memory_file(Memfile, read, Stream),
	    call_cleanup(load_structure(Stream, XML,
					[ dialect(xmlns),
					  xmlns('http://www.opengis.net/gml'),
					  xmlns(gml, 'http://www.opengis.net/gml')
					]),
			 free_data(Stream, Memfile)),
	    transform_gml(XML, Geom)
	;   construct_gml(GML, Geom)
	).

free_data(Stream, Memfile) :-
	close(Stream),
	free_memory_file(Memfile).

linearring('gml:LinearRing'('gml:posList'(LSC)),LR) :-
	phrase(poslist(LR),LinearRing),
	atom_codes(LSC,LinearRing).

interior([],[]).
interior(['gml:interior'(LR1)|T1],[LR2|T2]) :-
	linearring(LR1,LR2),
	interior(T1,T2).

construct_gml(GML,point(X,Y)) :-
	atomic_list_concat([X,Y],' ',PosList),
	phrase(html('gml:Point'('gml:pos'(PosList))),Atoms),
	atomic_list_concat(Atoms,GML).
construct_gml(GML,point(X,Y,Z)) :-
	atomic_list_concat([X,Y,Z],' ',PosList),
	phrase(html('gml:Point'('gml:pos'(PosList))),Atoms),
	atomic_list_concat(Atoms,GML).
construct_gml(GML,point(X,Y,Z,M)) :-
	atomic_list_concat([X,Y,Z,M],' ',PosList),
	phrase(html('gml:Point'('gml:pos'(PosList))),Atoms),
	atomic_list_concat(Atoms,GML).

construct_gml(GML,linestring(LS)) :-
	phrase(poslist(LS),LineString),
	atom_codes(LSC,LineString),
	phrase(html('gml:LineString'('gml:posList'(LSC))),Atoms),
	atomic_list_concat(Atoms,GML).

construct_gml(GML,polygon([Ext|Int])) :-
	linearring(ExtT,Ext),
	interior(InteriorTerms,Int),
	phrase(html('gml:Polygon'(['gml:exterior'(ExtT)|InteriorTerms])),Atoms),
	atomic_list_concat(Atoms,GML).

construct_gml(GML,box(point(X1,Y1),point(X2,Y2))) :-
	atomic_list_concat([X1,Y1],' ',PosList1),
	atomic_list_concat([X2,Y2],' ',PosList2),
	phrase(html('gml:Envelope'(['gml:lowerCorner'(PosList1),
				    'gml:upperCorner'(PosList2)])),Atoms),
	atomic_list_concat(Atoms,GML).

transform_gml(Elts,P) :-
	member(element(_:'Point',_,PointElts),Elts),
	get_point(PointElts,P).

transform_gml(Elts,linestring(LS)) :-
	member(element(_:'LineString',_,LineStringElts),Elts),
	get_linestring(LineStringElts,LS).

transform_gml(Elts,polygon([Ext|Int])) :-
	member(element(_:'Polygon',_,PolygonElts),Elts),
	get_polygon_exterior(PolygonElts,Ext),
	get_polygon_interiors(PolygonElts,Int).

transform_gml(Elts,box(Lower,Upper)) :-
	member(element(_:'Envelope',_,BoxElts),Elts),
	get_box(BoxElts,Lower,Upper).

get_point(Elts,P) :-
	xpath(Elts, //(_:'pos'), element(_,_,[A])),
	atom_codes(A,C),
	phrase(pos(P),C).

get_linestring(Elts,LS) :-
	xpath(Elts, //(_:'posList'), element(_,_,[A])),
	atom_codes(A,C),
	phrase(poslist(LS),C).

get_polygon_exterior(Polygon,Ext) :-
	xpath(Polygon, //(_:'exterior')/(_:'LinearRing')/(_:'posList'), element(_,_,[A])),
	atom_codes(A,C),
	phrase(poslist(Ext),C).
get_polygon_interiors(Polygon,Int) :-
	findall(I,get_polygon_interior(Polygon,I),Int).
get_polygon_interior(Polygon,Int) :-
	xpath(Polygon, //(_:'interior')/(_:'LinearRing')/(_:'posList'), element(_,_,[A])),
	atom_codes(A,C),
	phrase(poslist(Int),C).

get_box(Elts,LBC,UBC) :-
	xpath(Elts, //(_:'lowerCorner'), element(_,_,[LA])),
	xpath(Elts, //(_:'lowerCorner'), element(_,_,[UA])),
	atom_codes(LA,LC),
	atom_codes(UA,UC),
	phrase(pos(LBC),LC),
	phrase(pos(UBC),UC).


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







