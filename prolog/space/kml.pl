/*  Part of SWI-Prolog

    Author:        Willem Robert van Hage
    E-mail:        W.R.van.Hage@vu.nl
    WWW:           http://www.few.vu.nl/~wrvhage
    Copyright (c)  2009-2013, Vrije Universiteit Amsterdam
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

:- module(kml,
	  [
	    kml_shape/2,
	    kml_shape/4,
	    kml_uri_shape/3,
	    kml_file_shape/2,
	    kml_file_shape/4,
	    kml_file_uri_shape/3,
	    kml_save_header/2,
	    kml_save_shape/3,
	    kml_save_footer/1,
	    kml_file_to_georss/1,
	    kml_file_to_georss/2,
	    georss_to_kml_file/1,
	    georss_to_kml_file/2
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(dcg/basics)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(xpath)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(space/georss)).

:- rdf_register_ns(kml,'http://www.opengis.net/kml/2.2#').
:- rdf_meta store_element(r,r,?,t).

%%	kml_file_to_georss(+KMLfile) is det.
%%	kml_file_to_georss(+KMLfile,+RDFfile) is det.
%
%	Converts the contents of an KML file into GeoRSS RDF in
%	the RDF database of Prolog.
%       The Geometries are converted to GeoRSS properties and values.
%       Documents, Folders, etc. are ignored.
%	MultiGeometry objects are expanded into separate simple
%	Geometries.
%	Geometries with an XML ID are assigned that ID as URI,
%	other Geometries are assigned a RDF blank node.
%	The kml:name and kml:description are translated to RDF
%	properties.

kml_file_to_georss(KMLfile) :-
	findall(uri_shape(U,S), kml_file_uri_shape(KMLfile,U,S), US),
	forall(member(uri_shape(U,S), US),
	       (   georss_uri_shape_triple(U,S,Sub,Pred,Obj),
		   rdf_assert(Sub,Pred,Obj,KMLfile)
	       )).
kml_file_to_georss(KMLfile,RDFfile) :-
	kml_file_to_georss(KMLfile),
	rdf_save(RDFfile,[graph(KMLfile)]).


%%	georss_to_kml_file(+KMLfile) is det.
%%	georss_to_kml_file(+KMLfile,+Options) is det.
%
%	Converts the contents of the RDF database of Prolog
%	into a KML file without style information and without
%       Folders.
%	kml:name and kml:description properties in the RDF database are
%	converted to their KML counterparts.
%       Options can be used to pass Document level options,
%       for example, the name of the dataset. Options can also include
%       a graph(Graph) option to specify which RDF named graph should
%       be converted to KML.

georss_to_kml_file(KMLfile) :-
	georss_to_kml_file(KMLfile, []).
georss_to_kml_file(KMLfile, Options) :-
	option(graph(Graph),Options,-),
	open(KMLfile, write, Stream, Options),
	kml_save_header(Stream, Options),
	forall((   Graph \= -
	       ->  georss:georss_candidate(U,S,Graph)
	       ;   georss:georss_candidate(U,S)
	       ),
	       (   (   (   Graph \= -
		       ->  rdf(U,kml:name,literal(N),Graph)
		       ;   rdf(U,kml:name,literal(N))
		       )
		   ->  Name = [name(N)]
		   ;   Name = []
		   ),
	           (   (   Graph \= -
		       ->  (   rdf(U,kml:description,literal(type(rdf:'XMLLiteral',D)),Graph)
			   ->  true
			   ;   rdf(U,kml:description,literal(D),Graph)
			   )
		       ;   (   rdf(U,kml:description,literal(type(rdf:'XMLLiteral',D)))
			   ->  true
			   ;   rdf(U,kml:description,literal(D))
			   )
		       )
		   ->  Desc = [description(D)]
		   ;   Desc = []
		   ),
		   append([Name,Desc],Cont),
		   (   rdf_is_bnode(U)
		   ->  kml_save_shape(Stream,
				      placemark(S,[],Cont),
				      Options)
		   ;   kml_save_shape(Stream,
				      placemark(S,[geom_attributes([id(U)])],Cont),
				      Options)
		   )
	      )),
	kml_save_footer(Stream),
	close(Stream).

%%	kml_shape(?Stream,?Shape) is semidet.
%%	kml_shape(?Stream,?Shape,?Attributes,?Content) is semidet.
%
%	Converts between the KML serialization of a shape and its
%	internal Prolog term representation.
%	Attributes and Content can hold additional
%	attributes and XML content elements of the KML, like ID, name,
%	or styleUrl.

kml_shape(KML, Geom) :- kml_shape(KML, Geom, _A, _C).
kml_shape(KML, Geom, Attributes, Content) :-
	(   nonvar(KML)
	->  atom_to_memory_file(KML, Memfile),
	    open_memory_file(Memfile, read, Stream),
	    call_cleanup(load_structure(Stream, XML,
					[ dialect(xmlns),
					  xmlns('http://www.opengis.net/kml/2.2'),
					  xmlns(kml, 'http://www.opengis.net/kml/2.2')
					]),
			 free_data(Stream, Memfile)), !,
	    transform_kml(XML, Geom, Attributes, Content)
	;   construct_kml(KML, Geom, Attributes, Content)
	).

%%	kml_uri_shape(?KML,?URI,?Shape) is semidet.
%
%	Converts between the KML serialization of a URI-shape pair and its
%	internal Prolog term representation.
%	It is assumed the KML Geometry element has a ID attribute
%       specifying the URI of the shape. e.g.
%
%	    ==
%	    <PointID="http://example.org/point1"><coordinates>52.37,4.89</coordinates></Point>
%	    ==

kml_uri_shape(KML, URI, Shape) :-
	(   nonvar(KML)
	->  (   (   kml_shape(KML, Shape, Attr, _),
	            (	Shape = placemark(Shape, _, _)
		    ->  get_uri_shape(Shape, URI, _)
		    ;   memberchk(id=URI, Attr)
		    )
		)
	    )
	;   kml_shape(KML, Shape, [id=URI], [])
	).

%%	kml_file_shape(+File,?Shape) is semidet.
%%	kml_file_shape(+File,?Shape,?Attributes,?Content) is semidet.
%
%	Reads shapes from a KML file using kml_shape/2.
%	kml_file_shape/4 also reads extra attributes and elements of
%	the KML Geometry.
%	e.g. <Point targetId="NCName"><extrude>0</extrude>...</Point>
%	will, besides parsing the Point, also instantiate Content with
%       [extrude(0)] and Attributes with [targetId('NCName')].

kml_file_shape(File, Geom) :- kml_file_shape(File, Geom, _A, _C).
kml_file_shape(File, Geom, Attributes, Content) :-
	load_structure(File, XML,
		       [ dialect(xmlns),
%			 xmlns('http://www.opengis.net/kml/2.2'),
			 xmlns(kml, 'http://www.opengis.net/kml/2.2')
		       ]), !,
	transform_kml(XML, Geom, Attributes, Content).


%%	kml_file_uri_shape(+File,?URI,?Shape) is semidet.
%
%	Reads URI-shape pairs from File using kml_uri_shape/2.

kml_file_uri_shape(File, URI, Shape) :-
	kml_file_shape(File, Geom, _Attributes, _Content),
	get_uri_shape(Geom, URI, Shape, File).

% work-around hack to avoid implementing geometry collections in C++
non_geometrycollection_member(Shape, Geoms) :-
	member(Member,Geoms),
	(   Member = geometrycollection(Content)
	->  non_geometrycollection_member(Shape,Content)
	;   Shape = Member
	).
get_shape(Shape,Shape2) :-
	(   Shape = geometrycollection(Content)
	->  non_geometrycollection_member(Shape2,Content)
	;   Shape2 = Shape
	).

get_uri_shape(E,U,S) :-
	get_uri_shape(E,U,S,_).
get_uri_shape(geometrycollection(Content), URI, Shape, File) :-
	non_geometrycollection_member(Member, Content),
	get_uri_shape(Member, URI, Shape, File).
get_uri_shape(document([H|T]), URI, Shape, File) :-
	get_uri_shape(H, URI, Shape, File) ;
	get_uri_shape(document(T), URI, Shape, File).
get_uri_shape(folder([H|T]), URI, Shape, File) :-
	get_uri_shape(H, URI, Shape, File) ;
	get_uri_shape(folder(T), URI, Shape, File).
get_uri_shape(folder([H|T],_,_), URI, Shape, File) :-
	get_uri_shape(H, URI, Shape, File) ;
	get_uri_shape(folder(T), URI, Shape, File).
get_uri_shape(placemark(Shape,Attributes,_),URI,Shape2, _File) :-
	member(geom_attributes(GA),Attributes),
	(   memberchk(id=URI, GA) ;
	    memberchk('ID'=URI, GA)
	), !,
	get_shape(Shape,Shape2).
get_uri_shape(placemark(Shape,_,E), URI, Shape2, File) :-
	(   member(description([D]), E)
	->  (   atom(D),
	        once(atom_codes(D, DC)),
		phrase(uri(URIcodes),DC,_)
	    ->  atom_codes(URI,URIcodes)
	    ;	rdf_bnode(URI)
	    ), store_element(URI, kml:description, E, File)
	;   rdf_bnode(URI)
	), store_element(URI, kml:name, E, File),
	get_shape(Shape,Shape2).

store_element(URI, Prop, Element, Graph) :-
	(   E =.. [Prop,[D]]
	;   rdf_global_id(_NS:ID,Prop),
	    E =.. [ID,[D]]
	),
	member(E, Element),
	atom(D),
	(   atom_to_memory_file(D, Memfile),
	    open_memory_file(Memfile, read, Stream),
	    call_cleanup(load_structure(Stream, XML,
					[ dialect(xmlns),
					  syntax_errors(quiet) ]),
			 free_data(Stream, Memfile)),
	    memberchk(element(_,_,_),XML)
	->  rdf_equal(rdf:'XMLLiteral', XMLType),
	    Literal = type(XMLType, XML)
	;   Literal = D
	),
	(   nonvar(Graph)
	->  rdf_assert(URI, Prop, literal(Literal), Graph)
	;   rdf_assert(URI, Prop, literal(Literal))
	).

uri(URI) -->
	string(_), "http://", nonuri(Rest),
	{ string_codes("http://", Prefix),
	  append(Prefix, Rest, URI)
	}.

nonuri([H|T]) -->
	[H],
	{ code_type(H, graph),
	  H \== 0'<,
	  H \== 0')			% FIXME: make this a bit more subtle
	}, !,
	nonuri(T).
nonuri([]) -->
	[].


free_data(Stream, Memfile) :-
	close(Stream),
	free_memory_file(Memfile).

coordinates(point(X,Y),PosList) :-
	atomic_list_concat([Y,X], ',', PosList).
coordinates(point(X,Y,Z),PosList) :-
	atomic_list_concat([Y,X,Z], ',', PosList).

coordinates_list(Points, PosList) :-
	coordinates_list_aux(Points, PosLists),
	atomic_list_concat(PosLists, ' ', PosList).
coordinates_list_aux([],[]).
coordinates_list_aux([H|T],[Coord|Coords]) :-
	coordinates(H, Coord),
	coordinates_list_aux(T,Coords).

linearring(Points, Ring) :-
	(   nth0(0, Points, First1),
	    last(Points, First1)
	->  Ring = Points
	;   nth0(0, Points, First2),
	    append(Points, [First2], Ring)
	).

interior_tags([],[]).
interior_tags([PosList|PosLists],
	      ['LinearRing'('coordinates'(PosList))|ILRS]) :-
	interior_tags(PosLists, ILRS).

expand_nl([],[]).
expand_nl([nl|T],['\n'|T2]) :-
	expand_nl(T,T2).
expand_nl([nl(0)|T],[''|T2]) :-
	expand_nl(T,T2).
expand_nl([nl(N)|T],['\n'|T2]) :-
	M is N - 1,
	expand_nl([nl(M)|T],T2).
expand_nl([H|T],[H|T2]) :-
	expand_nl(T,T2).

construct_kml(KML, Geom) :- construct_kml(KML, Geom, [], []).

construct_kml(KML, Geom, Attributes, Content) :-
	construct_term(Geom, Attributes, Content, T),
	phrase(html(T), Atoms),
	expand_nl(Atoms,Atoms2),
	atomic_list_concat(Atoms2, KML).

construct_term(Geom, Attributes, Content, T) :-
	(   folder_term(Geom, Attributes, Content, T)
	;   placemark_term(Geom, Attributes, Content, T)
	;   point_term(Geom, Attributes, Content, T)
	;   linestring_term(Geom, Attributes, Content, T)
	;   linearring_term(Geom, Attributes, Content, T)
	;   polygon_term(Geom, Attributes, Content, T)
	;   multigeometry_term(Geom, Attributes, Content, T)
	).

point_term(Point, Attributes, Content, P) :-
	coordinates(Point, PosList),
	P = 'Point'(Attributes,[ 'coordinates'(PosList)|Content]).

linestring_term(linestring(Points), Attributes, Content, LR) :-
	coordinates_list(Points, PosList),
	LR = 'LineString'(Attributes, [ 'coordinates'(PosList) | Content ]).

linearring_term(linestring(Points), Attributes, Content, LR) :-
	linearring(Points, Ring),
	coordinates_list(Ring, PosList),
	LR = 'LinearRing'(Attributes, [ 'coordinates'(PosList) | Content ]).

polygon_term(polygon([ExternalRing|InternalRings]),
	     Attributes, Content, PT) :-
	linearring(ExternalRing, ER),
	coordinates_list(ER, ExtPosList),
	maplist(coordinates_list, InternalRings, IntPosLists),
	interior_tags(IntPosLists, ILRS),
	(   ILRS = []
	->  Rest = Content
	;   append(['innerBoundaryIs'(ILRS)], Content, Rest)
	),
	PT = 'Polygon'(Attributes, [ 'outerBoundaryIs'('LinearRing'('coordinates'(ExtPosList))) | Rest ]).

multigeometry_term(geometrycollection(GCS),
		   Attributes, Content, GCT) :-
	multigeometry_term_aux(GCS, GCTS),
	append(GCTS, Content, GCTS_and_Content),
	GCT = 'MultiGeometry'(Attributes, GCTS_and_Content).

multigeometry_term_aux([], []).
multigeometry_term_aux([Geom|Geoms], [T|Ts]) :-
	construct_term(Geom, [], [], T),
	multigeometry_term_aux(Geoms, Ts).

% FIXME: prehaps switch around [],[] and A,C?
placemark_term(placemark(Geom), A, C, T) :-
	placemark_term(placemark(Geom, [], []), A, C, T).

placemark_term(placemark(Geom, Attributes, Content), _A, _C, T) :-
	(   member(geom_attributes(_), Attributes)
	->  select(geom_attributes(GeomAttr), Attributes, PMAttr)
	;   GeomAttr = [],
            PMAttr = Attributes
	),
	(   member(geom_content(_), Content)
	->  select(geom_content(GeomCont), Content, PMCont)
	;   GeomCont = [],
            PMCont = Content
	),
	construct_term(Geom, GeomAttr, GeomCont, GT),
	T = 'Placemark'(PMAttr, [ GT | PMCont ]), !.

folder_term(folder(PMs), A, C, T) :-
	folder_term(folder(PMs,[],[]), A, C, T).

folder_term(folder(PMs, Attributes, Content), _A, _C, T) :-
	construct_term_list(PMs, PMTs),
	append(PMTs,Content,PMTsContent),
	T = 'Folder'(Attributes, PMTsContent).

construct_term_list([],[]).
construct_term_list([H|T],[HT|TT]) :-
	construct_term(H,[],[],HT),
	construct_term_list(T,TT).

% -----

transform_kml(Elts, P, Attributes, Content) :-
	member(element(_:'Point',A,PointElts), Elts),
	get_point(PointElts, P),
	get_extras([element(_,A,PointElts)], Attributes, Content).

transform_kml(Elts, linestring(P), Attributes, Content) :-
	(   member(element(_:'LineString',A,LSE), Elts)
	;   member(element(_:'LinearRing',A,LSE), Elts)
	),
        get_linestring(LSE, P),
	get_extras([element(_,A,LSE)], Attributes, Content).

transform_kml(Elts, polygon([Ext|Int]), Attributes, Content) :-
	member(element(_:'Polygon',A,PolygonElts), Elts),
	get_polygon_exterior(PolygonElts, Ext),
	get_polygon_interiors(PolygonElts, Int),
	get_extras([element(_,A,PolygonElts)], Attributes, Content).

transform_kml(Elts, geometrycollection(Geoms), Attributes, Content) :-
	member(element(_:'MultiGeometry',_,GeomElts), Elts),
	get_geometry(GeomElts, Geoms),
	get_extras(Elts, Attributes, Content).
/*
transform_kml(Elts, Geom, Attributes, Content) :-
	member(element(_:'MultiGeometry',_,GeomElts), Elts),
	get_geometry(GeomElts, Geoms),
	member(Geom, Geoms),
	get_extras(Elts, Attributes, Content).
*/
transform_kml(Elts, placemark(Geom, Attributes, Content), _A, _C) :-
	member(element(_:'Placemark',_,GeomElts), Elts),
	transform_kml(GeomElts, Geom, GeomAttr, GeomCont),
	get_extras(Elts, PMAttributes, PMContent),
	(   GeomAttr \= []
	->  append(PMAttributes,[geom_attributes(GeomAttr)],Attributes)
	;   Attributes = PMAttributes
	),
	(   GeomCont \= []
	->  append(PMContent,[geom_content(GeomCont)],Content)
	;   Content = PMContent
	), !.

transform_kml(Elts, folder(PMs, Attributes, Content), _, _) :-
	member(element(_:'Folder', _, PMElts), Elts),
	get_geometry(PMElts, PMs),
	get_extras(Elts, Attributes, Content), !.

transform_kml(Elts, PM, Attributes, Content) :-
	member(element(_:'Folder', _, PMElts), Elts),
	get_geometry(PMElts, PMs),
	get_extras(Elts, Attributes, Content),
	member(PM,PMs).

transform_kml(Elts, PM, Attributes, Content) :-
	member(element(_:'Document',_,PMElts), Elts),
	get_geometry(PMElts, PMs),
	get_extras(Elts, Attributes, Content),
	member(PM,PMs).

transform_kml(Elts, Doc, Attributes, Content) :-
	member(element(_:'kml',_,PMElts), Elts),
	transform_kml(PMElts, Doc, _, _),
	get_extras(Elts, Attributes, Content).

get_geometry([],[]).

get_geometry([Elt|Elts], [Geom|Geoms]) :-
	transform_kml([Elt], Geom, _A, _C),
	get_geometry(Elts, Geoms), !.
get_geometry([_|Elts], Geoms) :-
	get_geometry(Elts, Geoms).

get_point(Elts, P) :-
	xpath(Elts, //(_:'coordinates'), element(_,_,[A])),
	atom_codes(A, C),
	phrase(pos(P), C).

get_linestring(Elts, P) :-
	xpath(Elts, //(_:'coordinates'), element(_,_,[A])),
	atom_codes(A, C),
	phrase(poslist(P), C).

get_polygon_exterior(Elts, Ext) :-
	xpath(Elts, //(_:'outerBoundaryIs')//(_:'coordinates'), element(_,_,[A])),
	atom_codes(A, C),
	phrase(poslist(Ext), C).

get_polygon_interiors(Elts, Int) :-
	findall(I,
		(   xpath(Elts, //(_:'innerBoundaryIs')//(_:'coordinates'), element(_,_,[A])),
		    atom_codes(A, C),
		    phrase(poslist(I), C)
		),
		Int).

controlled_kml_term('Point').
controlled_kml_term('LineString').
controlled_kml_term('LinearRing').
controlled_kml_term('Polygon').
controlled_kml_term('innerBoundaryIs').
controlled_kml_term('outerBoundaryIs').
controlled_kml_term('MultiGeometry').
controlled_kml_term('coordinates').
controlled_kml_term('Placemark').
controlled_kml_term('Folder').


get_extras(Elts, Attributes, Cont) :-
	get_extras(Elts, IDs, Rests, Cont),
	append(IDs, Rests, Attributes), !.
get_extras(Elts, IDAttr, RestAttr, Content) :-
	member(element(_,_,C), Elts),
	filter_controlled(C, Content),
	member(element(_,A,_), Elts),
	filter_id(A, IDAttr, RestAttr), !.

filter_controlled([],[]).
filter_controlled([element(_:H,_A,C)|T], List) :-
	(   controlled_kml_term(H)
	->  filter_controlled(T,List)
	;   (   Tag =.. [H,C],
	        List = [Tag|U],
	        filter_controlled(T,U)
	    )
	), !.
filter_controlled([_|T],U) :-
	filter_controlled(T,U).

filter_id([],[],[]).
filter_id([ID=I|T],[ID=I|U],V) :-
	(   ID = 'id'
	;   ID = 'ID'
	),
	filter_id(T,U,V), !.
filter_id([H|T],U,[H|V]) :-
	filter_id(T,U,V).


poslist(T) --> blank_star, poslist_aux(T), blank_star, !.
poslist_aux(L) --> poslist_plus(L).
poslist_plus([H|T]) --> pos(H), poslist_star(T).
poslist_star(T) --> blank_plus, poslist_aux(T).
poslist_star([]) --> [], !.

pos(point(X,Y)) --> c(Y), ",", blank_star, c(X), ",", blank_star, "0".
pos(point(X,Y)) --> c(Y), ",", blank_star, c(X).
pos(point(X,Y,Z)) --> c(Y), ",", blank_star, c(X), ",", blank_star, c(Z).
pos(point(X,Y,Z,M)) --> c(Y), ",", blank_star, c(X), ",", blank_star, c(Z), ",", blank_star, c(M).
c(X) --> float(X).

blank_plus --> blank, blank_star, !.
blank_plus --> " ", !.
blank_star --> blanks, !.
blank_star --> [], !.



%%	kml_save_header(+Stream,+Options) is semidet.
%
%	Outputs a KML header to Stream.
%	This can be followed by calls to kml_save_shape/3 and
%	kml_save_footer/1.
%
%	Options is an option list that can contain the option name(Name)
%	specifying the Name of the document.
%
%	@tbd options to configure optional entities, like styles

kml_save_header(Stream,Options) :-
	format(Stream,"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",[]),
	format(Stream,"<kml xmlns=\"http://www.opengis.net/kml/2.2\">\n",[]),
	format(Stream,"<Document>\n",[]),
	(   stream_property(Stream,file_name(FName)),
	    option(name(Name),Options,FName)
	->  format(Stream,"<name>~w</name>\n",[Name])
	;   option(name(Name),Options),
	    atom(Name)
	->  format(Stream,"<name>~w</name>\n",[Name])
	;   true
	).


%%	kml_save_shape(+Stream,+Shape,+Options) is semidet.
%
%	Outputs a KML serialization of Shape to Stream.
%	This can be preceded by a call to kml_save_header/2 and
%	followed by more calls to kml_save_shape/3 and a call to
%	kml_save_footer/1.
%
%	Options is an option list that can contain the option
%	attr(+List) or content(+List) that can be used to add
%	additional attributes or xml element content to a shape.
%	This can be used to specify things like the ID or name.
%
%	Layout elements, like Placemark and Folder, have their
%       own separate extra attributes to supply additional
%       attributes and content.
%       These can contain the special terms geom_attributes and
%	geom_content that pass their content to the shape contained by
%	the Placemark.
%       For example, rendering a Placemark with the ID
%	"placemark12" of an extruded Point shape with its URI as name of
%	the Placemark and as ID of the shape and an additional styleUrl
%	works as follows:
%
%       ==
%       kml_save_shape(Stream,
%                      placemark(point(53.0,3.9),
%				 [ id(placemark12),
%				   geom_attributes([ id(URI) ])
%                                ],
%                                [ name(URI),styleUrl(URI),
%				   geom_content([ extrude(1) ])
%                                ]),
%                      []).
%	==

kml_save_shape(Stream,Shape,Options) :-
	option(attr(Attributes),Options,[]),
	option(content(Content),Options,[]),
	kml_shape(KML,Shape,Attributes,Content),
	format(Stream,"~w\n",[KML]).

%%	kml_save_footer(+Stream) is det.
%
%	Outputs a KML footer to stream Stream.
%	This can be preceded by calls to kml_save_header/2 and
%	kml_save_shape/3.

kml_save_footer(Stream) :-
	format(Stream,"\n</Document>\n</kml>\n\n",[]).

