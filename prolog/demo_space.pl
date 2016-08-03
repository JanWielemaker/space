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

% load the Prolog module that uses the space shared object file
:- use_module(library(space/space)).

% load the semantic web package
:- use_module(library('semweb/rdf_db.pl')).

% eye candy, declare shorthand for namespaces
:- rdf_register_ns(geo, 'http://www.geonames.org/ontology#').
:- rdf_register_ns(wgs84, 'http://www.w3.org/2003/01/geo/wgs84_pos#').

:- writef("\n----\n\nLoading demo RDF file of Geonames features in the Rotterdam harbor.\n").
% load the demo RDF file containing Geonames features around the Rotterdam harbor
:- rdf_load('demo_geonames.rdf').

% this adds all URIs with associated coordinates to the space indexing queue
:- writef("Selecting features with coordinates to put in the spatial index.\n").
:- space_bulkload(uri_shape,'demo_index').
:- writef("done loading demo\n\n----\n\n").

% find Features in order of proximity to the point Lat Long
nearest_features(point(Lat,Long), Name) :-
        space_nearest(point(Lat,Long), Nearest,'demo_index'),
        rdf(Nearest, rdf:type, geo:'Feature'), % atoms starting with capitals have to be quoted.
        rdf(Nearest, geo:name, literal(Name)).

% find Features contained in the box defined by the two points        
contained_features(box(point(NWLat,NWLong),point(SELat,SELong)), Name) :-
        space_contains(box(point(NWLat,NWLong),point(SELat,SELong)), Contained, 'demo_index'),
        rdf(Contained, rdf:type, geo:'Feature'),
        rdf(Contained, geo:name, literal(Name)).

% find Features in order of proximity, but restrict them to those with featureCode harbor
% also, fetch and show their coordinates
nearest_harbors(point(Lat,Long), Name, point(HarborLat,HarborLong)) :-
        space_nearest(point(Lat,Long), Nearest,'demo_index'),
        rdf(Nearest, rdf:type, geo:'Feature'),
        rdf(Nearest, geo:featureCode, geo:'H.HBR'),
        rdf(Nearest, geo:name, literal(Name)),
        rdf(Nearest, wgs84:lat, literal(HarborLat)),
        rdf(Nearest, wgs84:long, literal(HarborLong)).



:- writef("Welcome to the SWI-Prolog \"space\" package demo.\n\n").
:- writef("Try finding features in the Rotterdam harbor.\n").
:- writef("To find features near lat 51.96 long 4.13, try the following:\n").
:- writef("?- nearest_features(point(51.96,4.13), Name).\n\n").
:- writef("To find the nearest harbor to that point, try the following:\n").
:- writef("?- nearest_harbors(point(51.96,4.13), Name, point(HarborLat,HarborLong)).\n\n").
:- writef("To find features in the rectangular area between ").
:- writef("lat 51.93 long 4.10 and lat 51.96 long 4.19, try the following:\n").
:- writef("?- contained_features(box(point(51.93,4.10),point(51.96,4.19)), Name).\n\n").
:- writef("Have fun experimenting with the \"space\" package!\n").
:- writef("If you have questions, please e-mail me.\n\n").
:- writef("Willem Robert van Hage <W.R.van.Hage@vu.nl>\n\n").

