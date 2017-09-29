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

:- module(freebase,
          [  freebase_candidate/2,
             freebase_candidate/3
          ]).

:- use_module(library(semweb/rdf_db)).
:- rdf_register_ns(fb, 'http://rdf.freebase.com/ns/').

:- rdf_meta(freebase_candidate(r,?)).
:- rdf_meta(freebase_candidate(r,?,?)).

%!  freebase_candidate(?URI,?Point) is nondet.
%
%   Finds URI-Shape pairs of RDF resource that are place-tagged with
%   Freebase's location.location.geoposition notation that capture
%   WGS84 latitude/longitude positions.

freebase_candidate(URI,point(Lat,Long)) :-
    rdf(URI,fb:'location.location.geolocation',BN),
    rdf(BN,fb:'location.geocode.latitude',literal(type(_,LatAtom))),
    rdf(BN,fb:'location.geocode.longitude',literal(type(_,LongAtom))),
    atom_number(LatAtom,Lat),
    atom_number(LongAtom,Long).

%!  freebase_candidate(?URI,?Point,?Source) is nondet.
%
%   Finds URI-Shape pairs of RDF resource that are place-tagged with
%   Freebase's location.location.geoposition notation that capture
%   WGS84 latitude/longitude positions.
%   From RDF that was loaded from a certain Source.

freebase_candidate(URI,point(Lat,Long),Source) :-
    rdf(URI,fb:'location.location.geolocation',BN,Source),
    rdf(BN,fb:'location.geocode.latitude',literal(type(_,LatAtom))),
    rdf(BN,fb:'location.geocode.longitude',literal(type(_,LongAtom))),
    atom_number(LatAtom,Lat),
    atom_number(LongAtom,Long).




