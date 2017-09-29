:- use_module(library(plunit)).
:- use_module(library(space/space)).
:- use_module(library(semweb/rdf_db)).

:- rdf_register_ns(poseidon,'http://semanticweb.cs.vu.nl/poseidon/ns/instances/').




:- begin_tests(space, [ setup( rdf_load('test/clearways') ),
                        cleanup( rdf_reset_db ) ]).

test(space_clear, [ fail ]) :-
    space_clear(test_index),
    space_nearest(point(0.0,0.0),_N,test_index).

test(space_bulkload, [cleanup(space_clear(test_index))]) :-
    space_bulkload(uri_shape, test_index).

test(space_nearest, [cleanup(space_clear(test_index))]) :-
    space_bulkload(uri_shape, test_index),
    space_nearest(point(0.0,0.0),_N,test_index), !.

test(uri_shape1,[ Maas1Shape == Maas1OKShape ]) :-
    rdf_global_id(poseidon:'ScheepvaartrouteMaas_1', Maas1URI),
    Maas1OKShape = polygon([[point(51.8622383341678, 3.190847055031),
                             point(51.9264073767511, 3.17634603939213),
                             point(51.9439080037163, 3.3030218335942),
                             point(51.9830760341423, 3.59004255199478),
                             point(52.0072435082724, 3.7700555741797),
                             point(51.9740756071301, 3.77705603537945),
                             point(51.9609084845253, 3.70971782897821),
                             point(51.940574456261,  3.59920983978603),
                             point(51.8972395893234, 3.3725268083187),
                             point(51.8917393958784, 3.34369138934054),
                             point(51.8864058754285, 3.31585606291782),
                             point(51.8622383341678, 3.190847055031)]]),
    uri_shape(Maas1URI,Maas1Shape),
    !.

test(uri_shape2,[ Maas4Shape == Maas4OKShape ]) :-
    rdf_global_id(poseidon:'ScheepvaartrouteMaas_4', Maas4URI),
    Maas4OKShape = polygon([ [ point(52.083246342296,  3.75388786172814),
                               point(52.3710752345553, 3.01216768774432),
                               point(52.417759041374,  3.05833759278454),
                               point(52.4389087756074, 3.07117653902477),
                               point(52.0969134612883, 3.85006149283663),
                               point(52.083246342296,  3.75388786172814)
                             ]
                           ]),
    uri_shape(Maas4URI,Maas4Shape),
    !.

test(space_intersects,[cleanup(space_clear(test_index))]) :-
    space_bulkload(uri_shape, test_index),
    rdf_global_id(poseidon:'ScheepvaartrouteMaas_1',Maas1URI),
    rdf_global_id(poseidon:'ScheepvaartrouteMaas_4',Maas4URI),
    uri_shape(Maas1URI,Maas1Shape),
    uri_shape(Maas4URI,Maas4Shape),
    rdf_global_id(poseidon:'ScheepvaartrouteZuid_richting_noord',NoordURI),
    findall(Inter,space_intersects(Maas1Shape,Inter,test_index),Inters),
    \+member(NoordURI,Inters),
    !,
    space_intersects(Maas4Shape,NoordURI,test_index).

test(space_index, [cleanup(space_clear(test_index))]) :-
    space_bulkload(uri_shape, test_index),
    rdf_global_id(poseidon:testPoint1,P1),
    rdf_global_id(poseidon:testPoint2,P2),
    rdf_global_id(poseidon:testPoint3,P3),
    space_assert(P1,point(52.4389,3.07118),test_index),
    space_assert(P2,point(52.3983,3.13086),test_index),
    space_assert(P3,point(52.3254,3.06849),test_index),
    space:space_queue(test_index,assert,P1,point(52.4389,3.07118)),
    space_index(test_index),
    \+space:space_queue(test_index,assert,P1,point(52.4389,3.07118)),
    space_retract(P1,point(52.4389,3.07118),test_index),
    space_retract(P2,point(52.3983,3.13086),test_index),
    space_retract(P3,point(52.3254,3.06849),test_index),
    space:space_queue(test_index,retract,P1,point(52.4389,3.07118)),
    space_index(test_index),
    \+space:space_queue(test_index,retract,P1,point(52.4389,3.07118)),
    space_assert(P1,point(52.4389,3.07118),test_index),
    space_assert(P2,point(52.3983,3.13086),test_index),
    space_assert(P3,point(52.3254,3.06849),test_index),
    space_index(test_index),
    !.


test(space_contains,[cleanup(space_clear(test_index))]) :-
    space_bulkload(uri_shape, test_index),
    rdf_global_id(poseidon:'ScheepvaartrouteMaas_4',Maas4URI),
    rdf_global_id(poseidon:testPoint1,P1),
    rdf_global_id(poseidon:testPoint2,P2),
    rdf_global_id(poseidon:testPoint3,P3),
    space_assert(P1,point(52.4389,3.07118),test_index),
    space_assert(P2,point(52.3983,3.13086),test_index),
    space_assert(P3,point(52.3254,3.06849),test_index),
    uri_shape(Maas4URI,Maas4Shape),
    findall(C,space_contains(Maas4Shape,C,test_index),Cs),
    \+member(P3,Cs),
    !,
    space_contains(Maas4Shape,P1,test_index),
    space_contains(Maas4Shape,P2,test_index).


test(space_intersects,[cleanup(space_clear(test_index))]) :-
    space_bulkload(uri_shape, test_index),
    rdf_global_id(poseidon:'ScheepvaartrouteZuid_richting_noord',NoordURI),
    rdf_global_id(poseidon:'ScheepvaartrouteMaas_4',Maas4URI),
    rdf_global_id(poseidon:testPoint1,P1),
    rdf_global_id(poseidon:testPoint3,P3),
    space_assert(P1,point(52.4389,3.07118),test_index),
    space_assert(P3,point(52.3254,3.06849),test_index),
    uri_shape(Maas4URI,Maas4Shape),
    findall(Int,space_intersects(Maas4Shape,Int,test_index),Ints),
    \+member(P3,Ints),
    !,
    space_intersects(Maas4Shape,P1,test_index),
    space_intersects(Maas4Shape,NoordURI,test_index).

test(space_nearest, [ true(Pts = [P2,P1,P3]),
                      cleanup(space_clear(test_index)) ]) :-
    space_bulkload(uri_shape, test_index),
    rdf_global_id(poseidon:'Deep-draught_anchorage_Aanloopgebied_IJmuiden',DeepURI),
    rdf_global_id(poseidon:testPoint1,P1),
    rdf_global_id(poseidon:testPoint2,P2),
    rdf_global_id(poseidon:testPoint3,P3),
    space_assert(P1,point(52.4389,3.07118),test_index),
    space_assert(P2,point(52.3983,3.13086),test_index),
    space_assert(P3,point(52.3254,3.06849),test_index),
    uri_shape(DeepURI,DeepShape),
    findall(Pt,(space_nearest(DeepShape,Pt,test_index),
                rdf_global_id(poseidon:Lt,Pt),
                atom_concat(testPoint,_,Lt)), Pts),
    !.

test(space_retract, [ true(Ps = [P2,P3]),
                      cleanup(space_clear(test_index)) ]) :-
    space_bulkload(uri_shape, test_index),
    rdf_global_id(poseidon:'Deep-draught_anchorage_Aanloopgebied_IJmuiden',DeepURI),
    rdf_global_id(poseidon:testPoint1,P1),
    rdf_global_id(poseidon:testPoint2,P2),
    rdf_global_id(poseidon:testPoint3,P3),
    space_assert(P1,point(52.4389,3.07118),test_index),
    space_assert(P2,point(52.3983,3.13086),test_index),
    space_assert(P3,point(52.3254,3.06849),test_index),
    uri_shape(DeepURI,DeepShape),
    space_retract(P1,point(52.4389,3.07118),test_index),
    findall(P,(space_nearest(DeepShape,P,test_index),
               rdf_global_id(poseidon:L,P),
               atom_concat(testPoint,_,L)), Ps),
    !.

:- end_tests(space).

test_space :-
    run_tests(space).

