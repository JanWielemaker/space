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

:- module(wgs84,
          [  wgs84_candidate/2,
             wgs84_candidate/3,
             coordinates/3,
             coordinates/4,
             lat/2,
             long/2,
             alt/2
          ]).

:- use_module(library(semweb/rdf_db)).
:- rdf_register_ns(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- rdf_meta(wgs84_candidate(r,?)).
:- rdf_meta(wgs84_candidate(r,?,?)).

%!  wgs84_candidate(?URI,?Point) is nondet.
%
%   Finds URI-Shape pairs of RDF resources that are place-tagged
%   with W3C WGS84 properties (i.e. lat, long, alt).
%   Point = point(?Lat,?Long) ; Point = point(?Lat,?Long,?Alt).

wgs84_candidate(URI,point(Lat,Long)) :-
    wgs84_candidate(URI,point(Lat,Long),_).

wgs84_candidate(URI,point(Lat,Long,Alt)) :-
    wgs84_candidate(URI,point(Lat,Long,Alt),_).

%!  wgs84_candidate(?URI,?Point,+Source) is nondet.
%
%   Finds URI-Shape pairs of RDF resources that are place-tagged
%   with W3C WGS84 properties (i.e. lat, long, alt).
%   From RDF that was loaded from a certain Source.

wgs84_candidate(URI,point(Lat,Long),Source) :-
    nonvar(Source),
    \+alt(URI,_,_),
    lat(URI,Lat,Source),
    long(URI,Long,Source).
wgs84_candidate(URI,point(Lat,Long),Source) :-
    var(Source),
    \+alt(URI,_,_),
    lat(URI,Lat,Source1),
    (   Source1 = Source:_
    ->  true
    ;   Source = Source1
    ),
    long(URI,Long,Source).

wgs84_candidate(URI,point(Lat,Long,Alt),Source) :-
    lat(URI,Lat,Source),
    long(URI,Long,Source),
    alt(URI,Alt,Source).

%!  lat(?URI,?Lat) is nondet.
%
%   Finds the WGS84 latitude of resource URI (and vice versa)
%   using the rdf_db index. Lat is a number.

lat(URI,Lat) :-
    lat(URI,Lat,_).
lat(URI,Lat,Source) :-
    rdf(URI,wgs84:lat,literal(LatAtom),Source),
    (   LatAtom = type(_,LatVal)
    ->  (   atom(LatVal)
        ->  atom_number(LatVal,Lat)
        ;   Lat = LatVal
        )
    ;   atom_number(LatAtom,Lat)
    ).

%!  long(?URI,?Long) is nondet.
%
%   Finds the WGS84 longitude of resource URI (and vice versa)
%   using the rdf_db index. Long is a number.

long(URI,Long) :-
    long(URI,Long,_).
long(URI,Long,Source) :-
    rdf(URI,wgs84:long,literal(LongAtom),Source),
    (   LongAtom = type(_,LongVal)
    ->  (   atom(LongVal)
        ->  atom_number(LongVal,Long)
        ;   Long = LongVal
        )
    ;   atom_number(LongAtom,Long)
    ).

%!  alt(?URI,?Alt) is nondet.
%
%   Finds the WGS84 altitude of resource URI (and vice versa)
%   using the rdf_db index. Alt is a number.

alt(URI,Alt) :-
    alt(URI,Alt,_).
alt(URI,Alt,Source) :-
    rdf(URI,wgs84:alt,literal(AltAtom),Source),
    (   AltAtom = type(_,AltVal)
    ->  (   atom(AltVal)
        ->  atom_number(AltVal,Alt)
        ;   Alt = AltVal
        )
    ;   atom_number(AltAtom,Alt)
    ).

%!  coordinates(?URI,?Lat,?Long) is nondet.
%!  coordinates(?URI,?Lat,?Long,?Alt) is nondet.
%
%   Finds the WGS84 latitude, longitude and possibly altitude
%   of resource URI (and vice versa) using the rdf_db index.
%   Lat, Long, and Alt are numbers.

coordinates(URI,Lat,Long) :-
    wgs84_candidate(URI,point(Lat,Long)).

coordinates(URI,Lat,Long,Alt) :-
    wgs84_candidate(URI,point(Lat,Long,Alt)).







