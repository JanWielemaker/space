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

:- module(wkt,
          [ wkt_shape/2
          ]).

%!  wkt_shape(?WKT,?Shape) is semidet.
%
%   Converts between the WKT serialization of a Shape and
%   its native Prolog term representation.

wkt_shape(WKT,Shape) :-
    (   var(WKT)
    ->  phrase(geometry_tagged_text(Shape), WKTlist),
        atomic_list_concat(WKTlist, WKT)
    ;   tokenize_atom(WKT, WKTlist),
        phrase(geometry_tagged_text(Shape), WKTlist)
    ).

geometry_tagged_text(T) --> point_tagged_text(T) ;
    linestring_tagged_text(T) ;
    polygon_tagged_text(T) ;
    multipoint_tagged_text(T) ;
    multilinestring_tagged_text(T) ;
    multipolygon_tagged_text(T) ;
    geometrycollection_tagged_text(T).

point_tagged_text(T) -->
    ['POINT'], point_text(T).
linestring_tagged_text(linestring(T)) -->
    ['LINESTRING'], blank_star, linestring_text(T).
polygon_tagged_text(polygon(T)) -->
    ['POLYGON'], blank_star, polygon_text(T).
multipoint_tagged_text(multipoint(T)) -->
    ['MULTIPOINT'], blank_star, multipoint_text(T).
multilinestring_tagged_text(multilinestring(T)) -->
    ['MULTILINESTRING'], blank_star, multilinestring_text(T).
multipolygon_tagged_text(multipolygon(T)) -->
    ['MULTIPOLYGON'], blank_star, multipolygon_text(T).
geometrycollection_tagged_text(geometrycollection(T)) -->
    ['GEOMETRYCOLLECTION'], blank_star, geometrycollection_text(T).

point_text(point(empty)) --> ['EMPTY'], !.
point_text(point(X,Y,Z,M)) -->
    {nonvar(X)}, blank_plus, ['ZM'], blank_plus, ['('], blank_star, point(zm_point(X,Y,Z,M)), blank_star, [')'], ! ;
    blank_star, ['ZM'], blank_star, ['('], blank_star, point(zm_point(X,Y,Z,M)), blank_star, [')'].
point_text(point(X,Y,Z)) -->
    {nonvar(X)}, blank_plus, ['Z'], blank_plus, ['('], blank_star, point(z_point(X,Y,Z)), blank_star, [')'], ! ;
    blank_star, ['Z'], blank_star, ['('], blank_star, point(z_point(X,Y,Z)), blank_star, [')'].
point_text(point(X,Y)) --> blank_star, ['('], blank_star, point(xy_point(X,Y)), blank_star, [')'].
linestring_text(empty) --> ['EMPTY'], !.
linestring_text(T) --> ['('], blank_star, points(T), blank_star, [')'].
polygon_text(empty) --> ['EMPTY'], !.
polygon_text(T) --> ['('], blank_star, linestring_texts(T), blank_star, [')'].
multipoint_text(empty) --> ['EMPTY'], !.
multipoint_text(T) --> ['('], blank_star, point_texts(T), blank_star, [')'].
multilinestring_text(empty) --> ['EMPTY'], !.
multilinestring_text(T) --> ['('], blank_star, linestring_texts(T), blank_star, [')'].
multipolygon_text(empty) --> ['EMPTY'], !.
multipolygon_text(T) --> ['('], blank_star, polygon_texts(T), blank_star, [')'].
geometrycollection_text(empty) --> ['EMPTY'], !.
geometrycollection_text(T) --> ['('], blank_star, geometry_tagged_texts(T), blank_star, [')'].

points([point(X,Y)|T]) --> point(xy_point(X,Y)), points_star(T).
points([point(X,Y,Z)|T]) --> point(z_point(X,Y,Z)), points_star(T).
points([point(X,Y,Z,M)|T]) --> point(zm_point(X,Y,Z,M)), points_star(T).
points_star(T) --> [','], blank_star, points(T).
points_star([]) --> [], !.
point_texts([H|T]) --> point_text(H), point_texts_star(T).
point_texts_star(T) -->  [','], blank_star, point_texts(T).
point_texts_star([]) --> [], !.
linestring_texts([H|T]) --> linestring_text(H), linestring_texts_star(T).
linestring_texts_star(T) --> [','], blank_star, linestring_texts(T).
linestring_texts_star([]) --> [], !.
polygon_texts([H|T]) --> polygon_text(H), polygon_texts_star(T).
polygon_texts_star(T) -->  [','], blank_star, polygon_texts(T).
polygon_texts_star([]) --> [], !.
geometry_tagged_texts([H|T]) --> geometry_tagged_text(H), geometry_tagged_texts_star(T).
geometry_tagged_texts_star(T) --> [','], blank_star, geometry_tagged_texts(T).
geometry_tagged_texts_star([]) --> [], !.

point(zm_point(X,Y,Z,M)) -->
    {nonvar(X)}, c(X), blank_plus, c(Y), blank_plus, c(Z), blank_plus, c(M), ! ;
    c(X), blank_star, c(Y), blank_star, c(Z), blank_star, c(M).
point(z_point(X,Y,Z)) -->
    {nonvar(X)}, c(X), blank_plus, c(Y), blank_plus, c(Z), ! ;
    c(X), blank_star, c(Y), blank_star, c(Z).
point(xy_point(X,Y)) -->
    {nonvar(X)}, c(X), blank_plus, c(Y), ! ;
    c(X), blank_star, c(Y).
c(X) --> {var(X)}, [ X ].
c(X) --> {atom(X), atom_number(X,Xn)}, [ Xn ].
c(X) --> {number(X)}, [ X ].

blank_plus --> [' '], blank_star, !.
blank_star --> [], !.
blank_star --> blank_plus, !.

