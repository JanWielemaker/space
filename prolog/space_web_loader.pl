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

:- module(space_web_loader,
          [ space_load_url/1,      % +URL
	    space_load_url/2,	   % +URL, +Options
            space_unload_url/1,    % +URL
            space_unload_url/2,    % +URL, +Options
	    space_crawl_url/1,     % +URL
	    space_crawl_url/2,     % +URL, +Options
            space_uncrawl_url/1,   % +URL
            space_uncrawl_url/2    % +URL, +Options
	  ]).

:- use_module(library(space/space)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).

%%	space_load_url(+URL) is det.
%
%	Retrieve RDF over HTTP from a URL, load it in the rdf_db and
%	index all URI-Shape pairs that can be found in it into the
%	default index.

space_load_url(URL) :- space_load_url(URL,[]).

%%	space_load_url(+URL,+Options) is det.
%
%	Load using space_load_url/1, given extra options.
%
%	        * index(+IndexName)
%	        Index the URI-Shape pairs into index named IndexName.
%
%		* graph(+Graph)
%		Store the URI-Shape pairs in the named graph Graph.
%		The pairs are recorded as uri_shape(URI,Shape,Graph).

space_load_url(URL, Options) :-
	space_setting(rtree_default_index(DefaultIndex)),
	option(index(IndexName), Options, DefaultIndex),
	(   option(graph(Graph), Options)
	->  rdf_load(URL,Graph)
	;   rdf_load(URL)
	),
	Counter = counter(0),
	forall(uri_shape(URI, Shape, URL),
	       (   space_assert(URI, Shape, IndexName),
		   arg(1, Counter, N0),
		   N is N0 + 1,
		   nb_setarg(1, Counter, N)
	       )),
	arg(1, Counter, C),
	print_message(informational,space_load_url(C,IndexName)).

%%	space_unload_url(+URL) is det.
%
%	Unload the RDF that was fetched from URL and remove all
%	URI-Shape pairs that are contained in it from the default index.

space_unload_url(URL) :- space_unload_url(URL,[]).

%%	space_unload_url(+URL,+Options) is det.
%
%	Unload the RDF that was fetched from URL and remove all
%	URI-Shape pairs that are contained in it. Accepts extra options:
%
%		* index(+IndexName)
%               Remove from the index named IndexName.
%
%		* graph(+Graph)
%		Remove the URI-Shape pairs from the named graph Graph.

space_unload_url(URL, Options) :-
	space_setting(rtree_default_index(DefaultIndex)),
	option(index(IndexName), Options, DefaultIndex),
	option(graph(Graph), Options, URL),
	Counter = counter(0),
	forall(uri_shape(URI, Shape, URL),
	       (   space_retract(URI, Shape, IndexName),
		   arg(1, Counter, N0),
		   N is N0 + 1,
		   nb_setarg(1, Counter, N)
	       )),
	arg(1, Counter, C),
	print_message(informational,space_unload_url(C,IndexName)),
	rdf_unload(Graph).

:- multifile prolog:message//1.

prolog:message(space_load_url(0,_)) -->	[], !.
prolog:message(space_load_url(C,IndexName)) -->
	[ 'Added ~w URI-Shape ~w to ~w'-[C, P, IndexName] ],
	{ plural(C,P) }.

prolog:message(space_unload_url(0,_)) --> [], !.
prolog:message(space_unload_url(C,IndexName)) -->
	[ 'Removed ~w URI-Shape ~w from ~w'-[C, P, IndexName] ],
	{ plural(C,P) }.

plural(1,pair) :- !.
plural(_,pairs).

prolog:message(space_crawl_url(C)) -->
	[ 'Crawling ~w'-[C] ].

prolog:message(space_uncrawl_url(C)) -->
	[ 'Uncrawling ~w'-[C] ].


%%	link_property(+Property) is det.
%
%	RDF properties declared a link_property will be traversed by
%	space_crawl_url. link_property is a dynamic property.
%	By default owl:sameAs, skos:exactMatch, and skos:closeMatch are
%	link properties.

:- dynamic link_property/1.
link_property('http://www.w3.org/2002/07/owl#sameAs').
link_property('http://www.w3.org/2004/02/skos/core#exactMatch').
link_property('http://www.w3.org/2004/02/skos/core#closeMatch').

%%	space_crawl_url(+URL) is det.
%
%	Retrieve RDF over HTTP from a URL, load it in the rdf_db and
%	index all URI-Shape pairs that can be found in it into the
%	default index.
%	Also attempt to resolve all URIs that appear as object in a
%	link_property statement downloaded from the URL. Retrieve
%	these URIs and process them in the same way. Iterate this
%	process until there are no new links that have not already
%	been crawled.

space_crawl_url(URL) :-	space_crawl_url(URL,[]).

%%	space_crawl_url(+URL,+Options) is det.
%
%	Crawl using space_crawl_url/1, with additional options.
%
%               * index(+IndexName)
%		Index the URI-Shape pairs into index named IndexName.
%
%		* graph(+Graph)
%		Store the URI-Shape pairs in the named graph Graph.
%		The pairs are recorded as uri_shape(URI,Shape,Graph).

space_crawl_url(URL,Options) :-
	with_mutex(message,print_message(informational,space_crawl_url(URL))),
	space_load_url(URL,Options),
	findall( NewLink, new_link(URL:_,NewLink,_Type), NewLinks ),
	forall( member(NL, NewLinks),
	        thread_create(space_crawl_url(NL,Options),_,[])
	      ).

%%	space_uncrawl_url(+URL) is det.
%
%	Unload the RDF that was fetched from URL and remove all
%	URI-Shape pairs that are contained in it from the default index.
%	Also unload all data that were crawled by iteratively resolving
%	the URIs linked to with a link_property.

space_uncrawl_url(URL) :- space_uncrawl_url(URL,[]).

%%	space_uncrawl_url(+URL,+IndexName) is det.
%
%	Unload using space_uncrawl_url/1, but remove the URI-Shape pairs
%	from the index named IndexName.
%
%               * index(+IndexName)
%		Remove the URI-Shape pairs from index named IndexName.
%
%		* graph(+Graph)
%		Remove the URI-Shape pairs from the named graph Graph.

space_uncrawl_url(URL,Options) :-
	with_mutex(message,print_message(informational,space_uncrawl_url(URL))),
	findall( Link, old_link(URL:_,Link,_Type), Links ),
	space_unload_url(URL,Options),
	forall( member(L, Links),
		space_uncrawl_url(L,Options)
	      ).

new_link(FromSource,NewLink,P) :-
	link_property(P),
	rdf(_,P,NewLink,FromSource),
	\+once(rdf(_,_,_,NewLink:_)).

old_link(FromSource,Link,P) :-
	link_property(P),
	rdf(_,P,Link,FromSource),
	once(rdf(_,_,_,Link:_)).






