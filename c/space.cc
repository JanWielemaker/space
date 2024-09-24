/*  Part of SWI-Prolog

    Author:        Willem Robert van Hage
    E-mail:        wrvhage@few.vu.nl
    WWW:           http://www.few.vu.nl/~wrvhage
    Copyright (c)  2009-2015, Vrije Universiteit Amsterdam
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

#define PL_ARITY_AS_SIZE
#include "globals.h"
#include "Shapes.h"
#include "Index.h"
#include "Search.h"
#include <SWI-cpp2.h>
#include <iostream>
#include <string.h>


		 /*******************************
		 *	       LOCKING		*
		 *******************************/

#define RDLOCK(lock)			rdlock(lock)
#define WRLOCK(lock, allowreaders)	wrlock(lock, allowreaders)
#define LOCKOUT_READERS(lock)		lockout_readers(lock)
#define REALLOW_READERS(lock)		reallow_readers(lock)
#define WRUNLOCK(lock)			unlock(lock, FALSE)
#define RDUNLOCK(lock)			unlock(lock, TRUE)
#define LOCK_MISC(lock)			lock_misc(lock)
#define UNLOCK_MISC(lock)		unlock_misc(lock)
#define INIT_LOCK(lock)			init_lock(lock)


// TODO: use SWI-cpp2-atommap.h
map<atom_t,Index*> index_map;
rwlock index_map_lock;

static void index_clear(PlTerm indexname) {
  #ifdef DEBUGGING
  cout << "clearing " << (char*)indexname << endl;
  #endif
  if ( index_map_lock.writer != -1 ) INIT_LOCK(&index_map_lock);
  PlAtom idx_atom(indexname.as_atom());
  if ( !WRLOCK(&index_map_lock, FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  auto iter = index_map.find(idx_atom.unwrap());
  if (iter != index_map.end()) {
    Index *idx = iter->second;
    index_map.erase(iter);
    delete idx;
  }
  if (index_map.size() == 0) {
    cleanup_geos();
  }
  WRUNLOCK(&index_map_lock);
}

#if 0 /* not used! */
static RTreeIndex* assert_rtree_index(PlTerm indexname, double util, int nodesz) {
  if ( index_map_lock.writer != -1 ) INIT_LOCK(&index_map_lock);
  PlAtom idx_atom(indexname);
  RTreeIndex *rv = NULL;
  if ( !WRLOCK(&index_map_lock, FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return NULL;
  }
  auto iter = index_map.find(idx_atom.unwrap());
  if (iter != index_map.end()) {
    rv = dynamic_cast<RTreeIndex*>(iter->second);
  } else {
  #ifdef DEBUGGING
    cout << "did not find " << (char*)indexname << " creating new empty index" << endl;
  #endif
    rv = new RTreeIndex(indexname,util,nodesz);
    if (index_map.size() == 0) {
      init_geos();
    }
    index_map[idx_atom.unwrap()] = rv;
  }
  WRUNLOCK(&index_map_lock);
  return rv;
}
#endif

static RTreeIndex* assert_rtree_index(PlTerm indexname) {
  if ( index_map_lock.writer != -1 ) INIT_LOCK(&index_map_lock);
  PlAtom idx_atom(indexname.as_atom());
  RTreeIndex *rv = NULL;
  if ( !WRLOCK(&index_map_lock, FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return NULL;
  }
  auto iter = index_map.find(idx_atom.unwrap());
  if (iter != index_map.end()) {
    rv = dynamic_cast<RTreeIndex*>(iter->second);
  } else {
  #ifdef DEBUGGING
    cout << "did not find " << (char*)indexname << " creating new empty index" << endl;
  #endif
    rv = new RTreeIndex(indexname);
    if (index_map.size() == 0) {
      init_geos();
    }
    index_map[idx_atom.unwrap()] = rv;
  }
  WRUNLOCK(&index_map_lock);
  return rv;
}

PREDICATE(rtree_set_space,2)
{
  // Configuration parameters
  static PlAtom ATOM_rtree_utilization("rtree_utilization");
  static PlAtom ATOM_rtree_nodesize("rtree_nodesize");
  static PlAtom ATOM_rtree_storage("rtree_storage");
  static PlAtom ATOM_rtree_distance_function("rtree_distance_function");
  // Configuration values
  static PlAtom ATOM_pythagorean("pythagorean");
  static PlAtom ATOM_haversine("haversine");
  static PlAtom ATOM_memory("memory");
  static PlAtom ATOM_disk("disk");
#ifdef DEBUGGING
  cout << "setting " << (char*)A2 << " parameter of " << (char*)A1 << endl;
#endif
  RTreeIndex* idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A1));
  if (A2.name() == ATOM_rtree_utilization) {
    idx->utilization = A2[1].as_double();
  } else if (A2.name() == ATOM_rtree_nodesize) {
    idx->nodesize = A2[1].as_int();
  } else if (A2.name() == ATOM_rtree_storage) {
    if (A2[1].name() == ATOM_memory) {
      idx->storage = MEMORY;
    } else if (A2[1].name() == ATOM_disk) {
      idx->storage = DISK;
    } else {
      throw PlDomainError("space_option",A2);
    }
  } else if (A2.name() == ATOM_rtree_distance_function) {
    if (idx == NULL) PL_fail;
    if (A2[1].name() == ATOM_pythagorean) {
      idx->distance_function = PYTHAGOREAN;
    } else if (A2[1].name() == ATOM_haversine) {
      idx->distance_function = HAVERSINE;
    } else {
      throw PlDomainError("space_option",A2);
    }
  } else {
    throw PlDomainError("space_option",A2);
  }
  PL_succeed;
}


PREDICATE(rtree_clear,1)
{
  #ifdef DEBUGGING
  cout << "clearing " << (char*)A1 << endl;
  #endif
  index_clear(A1);
  PL_succeed;
}



// uri, shape, indexname
PREDICATE_NONDET(rtree_uri_shape,3)
{
  auto state = handle.context_unique_ptr<IteratorState>();
  try
    {
      RTreeIndex *idx = NULL;

      switch( handle.foreign_control() )
        {
        case PL_FIRST_CALL:
          idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A3));
          if (idx->tree == NULL) PL_fail;
	  state.reset(new IteratorState());
	  if (A1.is_variable()) {
            state->uri_id_range.first = idx->uri_id_multimap.begin();
	    state->uri_id_range.second = idx->uri_id_multimap.end();
            state->uri_id_iter = state->uri_id_range.first;
	  } else {
	    state->uri_id_range = idx->uri_id_multimap.equal_range(A1.as_atom().unwrap());
            state->uri_id_iter = state->uri_id_range.first;
	  }
          goto iterate;

        case PL_REDO:
          idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A3));
          if (idx->tree == NULL) PL_fail;
        iterate:
          if (state->uri_id_iter == state->uri_id_range.second) {
            PL_fail;
          } else {
	    PlCheckFail(A1.unify_atom(PlAtom(state->uri_id_iter->first)));
            PlTerm_var shape_term;
            if (idx->getShapeTerm(state->uri_id_iter->second,shape_term)) {
	      PlCheckFail(A2.unify_term(shape_term));
//              cout << "found id " << state->uri_id_iter->second << endl;
	    } else {
              PL_fail;
            }
            ++(state->uri_id_iter);
            PL_retry_address(state.release());
	  }

        case PL_PRUNED:
          PL_succeed;
        }


    }
  catch (Tools::Exception& e)
    {
      cerr << "******ERROR******" << endl;
      std::string s = e.what();
      cerr << s << endl;
      return FALSE;		// JW: TBD: map exception!
    }
  catch (std::exception& e)
    {
      cerr << "******ERROR******" << endl;
      cerr << "other exception" << endl;
      cerr << e.what() << endl;
      return FALSE;
    }

  return FALSE;
}

// indexname, uri, shape
PREDICATE(rtree_insert_object,3)
{
  #ifdef DEBUGGING
  cout << "inserting object " << (char*)A2 << " into " << (char*)A1 << endl;
  #endif
  RTreeIndex* idx = dynamic_cast<RTreeIndex*> (assert_rtree_index(A1));
  return idx->insert_single_object(A2,A3);
}

// index_name, candidate generating Prolog goal (of shape somepred(URI,Shape)), dimensionality
PREDICATE(rtree_bulkload,3)
{
  #ifdef DEBUGGING
  cout << "bulk loading of objects into " << (char*)A1 << endl;
  #endif
  RTreeIndex *idx = dynamic_cast<RTreeIndex*> (assert_rtree_index(A1));
  if(!idx->bulk_load(A2,A3.as_int())) PL_fail;
  cerr << "% Added " << idx->bulkload_tmp_id_cnt << " URI-Shape pairs to " << A1.as_string() << endl;
  PL_succeed;
}

PREDICATE(rtree_insert_list,2)
{
  #ifdef DEBUGGING
  cout << "inserting list of objects into " << (char*)A1 << endl;
  #endif
  PlTerm list = A2.copy_term_ref();
  PlTerm_var head;
  PlTerm_var uri_term;
  PlTerm_var shape_term;
  while( list.get_list(head, list) ) {
    PlAtom name_atom(PlAtom::null);
    size_t arity;
    if (!head.get_name_arity(&name_atom,&arity)) PL_fail;
    if (!head.get_arg(1,uri_term) ||
        !head.get_arg(2,shape_term)) PL_fail;
    RTreeIndex *idx = dynamic_cast<RTreeIndex*> (assert_rtree_index(A1));
    if (!idx->insert_single_object(uri_term,shape_term)) PL_fail;
  }
  PL_succeed;
}


PREDICATE(rtree_delete_object,3)
{
  #ifdef DEBUGGING
  cout << "deleting object " << (char*)A2 << " from " << (char*)A1 << endl;
  #endif
  RTreeIndex *idx = dynamic_cast<RTreeIndex*> (assert_rtree_index(A1));
  return idx->delete_single_object(A2,A3);
}


PREDICATE(rtree_delete_list,2)
{
  #ifdef DEBUGGING
  cout << "deleting list of objects from " << (char*)A1 << endl;
  #endif
  PlTerm list = A2.copy_term_ref();
  PlTerm_var head;
  PlTerm_var uri_term;
  PlTerm_var shape_term;
  while( list.get_list(head, list) ) {
    PlAtom name_atom(PlAtom::null);
    size_t arity;
    if (!head.get_name_arity(&name_atom,&arity)) PL_fail;
    if (!head.get_arg(1,uri_term) ||
        !head.get_arg(2,shape_term)) PL_fail;
    RTreeIndex *idx = dynamic_cast<RTreeIndex*> (assert_rtree_index(A1));
    if (!idx->delete_single_object(uri_term,shape_term)) PL_fail;
  }
  PL_succeed;
}


PREDICATE_NONDET(rtree_incremental_intersection_query,3)
{
  auto qs = handle.context_unique_ptr<IncrementalRangeStrategy>();
  try
    {
      RTreeIndex *idx = NULL;
      switch( handle.foreign_control() )
        {
        case PL_FIRST_CALL:
          idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A3));
          if (idx->tree == NULL) PL_fail;
          qs.reset(new IncrementalRangeStrategy(IntersectionQuery,idx->interpret_shape(A1),NULL,idx));
          goto iterate;

        case PL_REDO:
          idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A3));
          if (idx->tree == NULL) PL_fail;
        iterate:
          idx->tree->queryStrategy(*qs);
          if (!qs->result_found) {
            PL_fail;
          }
          {
            PlAtom result_atom(qs->result);
            PlCheckFail(A2.unify_atom(result_atom));
          }
          PL_retry_address(qs.release());

        case PL_PRUNED:
          PL_succeed;
        }

    }
  catch (Tools::Exception& e)
    {
      cerr << "******ERROR******" << endl;
      std::string s = e.what();
      cerr << s << endl;
      return -1;
    }
  catch (std::exception& e)
    {
      cerr << "******ERROR******" << endl;
      cerr << "other exception " << e.what() << endl;
      return -1;
    }

  PL_succeed;

}


PREDICATE_NONDET(rtree_incremental_containment_query,3)
{
  auto qs = handle.context_unique_ptr<IncrementalRangeStrategy>();
  try
    {
      RTreeIndex *idx = NULL;
      switch( handle.foreign_control() )
        {
        case PL_FIRST_CALL:
          idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A3));
          if (idx->tree == NULL) PL_fail;
          qs.reset(new IncrementalRangeStrategy(ContainmentQuery,idx->interpret_shape(A1),NULL,idx));
          goto iterate;

        case PL_REDO:
          idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A3));
          if (idx->tree == NULL) PL_fail;
        iterate:
          idx->tree->queryStrategy(*qs);
          if (!qs->result_found) {
            PL_fail;
          }
          {
            PlAtom result_atom(qs->result);
            PlCheckFail(A2.unify_atom(result_atom));
          }
          PL_retry_address(qs.release());
      case PL_PRUNED:
        PL_succeed;
      }


    }
  catch (Tools::Exception& e)
    {
      cerr << "******ERROR******" << endl;
      std::string s = e.what();
      cerr << s << endl;
      return -1;
    }
  catch (std::exception& e)
    {
      cerr << "******ERROR******" << endl;
      cerr << "other exception " << e.what() << endl;
      return -1;
    }

  PL_succeed;

}

PREDICATE_NONDET(rtree_incremental_nearest_neighbor_query,3)
{
  auto qs = handle.context_unique_ptr<IncrementalNearestNeighborStrategy>();
  try
    {
      RTreeIndex *idx = NULL;

      switch( handle.foreign_control() )
        {
        case PL_FIRST_CALL:
          idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A3));
          if (idx->tree == NULL) PL_fail;
          qs.reset(new IncrementalNearestNeighborStrategy(idx->interpret_shape(A1),NULL,idx));
          goto iterate;

        case PL_REDO:
          idx = dynamic_cast<RTreeIndex*>(assert_rtree_index(A3));
          if (idx->tree == NULL) PL_fail;

        iterate:
          idx->tree->queryStrategy(*qs);
          if (!qs->result_found) {
            PL_fail;
          }
          {
            PlAtom result_atom(qs->result);
            PlCheckFail(A2.unify_atom(result_atom));
          }
          PL_retry_address(qs.release());

        case PL_PRUNED:
          PL_succeed;
        }


    }
  catch (Tools::Exception& e)
    {
      cerr << "******ERROR******" << endl;
      std::string s = e.what();
      cerr << s << endl;
      return -1;
    }
  catch (std::exception& e)
    {
      cerr << "******ERROR******" << endl;
      cerr << "other exception" << endl;
      cerr << e.what() << endl;
      return -1;
    }

  PL_succeed;
}

PREDICATE(rtree_distance,4) {
  RTreeIndex *idx = dynamic_cast<RTreeIndex*> (assert_rtree_index(A1));
  unique_ptr<IShape> a(idx->interpret_shape(A2));
  unique_ptr<IShape> b(idx->interpret_shape(A3));
  double d = a->getMinimumDistance(*b);
  return A4.unify_float(d);
}

PREDICATE(rtree_display,1) {
  PrintVisitor pv;
  TraverseDepthFirst* qsd = new TraverseDepthFirst(&pv);
  RTreeIndex *idx = dynamic_cast<RTreeIndex*> (assert_rtree_index(A1));
  idx->tree->queryStrategy(*qsd);
  PL_succeed;
}

PREDICATE(rtree_display_mbrs,1) {
  PrintGnuplotVisitor pgv;
  TraverseBreadthFirst* qsd = new TraverseBreadthFirst(&pgv);
  RTreeIndex *idx = dynamic_cast<RTreeIndex*> (assert_rtree_index(A1));
  idx->tree->queryStrategy(*qsd);
  PL_succeed;
}

using namespace geos::geom;
LinearRing*
create_square_linearring(double xoffset, double yoffset, double side) {
  CoordinateArraySequence *cl = new CoordinateArraySequence();
  cl->add(Coordinate(xoffset, yoffset));
  cl->add(Coordinate(xoffset, yoffset+side));
  cl->add(Coordinate(xoffset+side, yoffset+side));
  cl->add(Coordinate(xoffset+side, yoffset));
  cl->add(Coordinate(xoffset, yoffset));
  PrecisionModel *pm = new PrecisionModel(geos::geom::PrecisionModel::FLOATING);
  GeometryFactory::Ptr global_factory = GeometryFactory::create(pm, -1);
  LinearRing *lr = global_factory->createLinearRing(cl);
  return lr;
}

geos::geom::Polygon*
create_square_polygon(double xoffset, double yoffset, double side) {
  LinearRing *outer = create_square_linearring(xoffset,yoffset,side);
  LinearRing *inner = create_square_linearring(xoffset+(side/3),
                                               yoffset+(side/3),(side/3));

  vector<LinearRing *> *holes  = new vector<LinearRing *>;
  holes->push_back(inner);
  /* vector<Geometry *> *holes = new vector<Geometry *>; */
  /* holes->push_back((Geometry*)inner); */
  /* PrecisionModel *pm = new PrecisionModel(geos::geom::PrecisionModel::FLOATING); */
  /* GeometryFactory::Ptr global_factory = GeometryFactory::create(pm, -1); */
  geos::geom::Polygon *poly = global_factory->createPolygon(outer, holes);
  return poly;
}

PREDICATE(geos_test,0) {
  init_geos();
  cout << "entering" << endl;
  geos::geom::Coordinate c(4.0, 2.0);
  cout << "created Coordinate" << endl;
  GEOSPoint *p = new GEOSPoint(c);
  GEOSPoint *q = new GEOSPoint(c);
  cout << "created GEOSPoint " << p << endl;
  cout << "testing GEOSPoint coordinates " << p->g->getCoordinate()->x << endl;
  double coords[2];
  coords[0] = 4.0;
  coords[1] = 2.0;
  SpatialIndex::Point *pt = new SpatialIndex::Point(coords,2);
  cout << "created Point" << endl;

  cout << "size of a GEOSPoint " << sizeof(*p) << endl;
  cout << "size of a geom::geos::Point " << sizeof(*(p->g)) << endl;
  cout << "size of a SpatialIndex::Point " << sizeof(*pt) << endl;


  if (p->intersectsShape((GEOSPoint&)*q))
    cout << "intersects GEOSPoint!" << endl;
  if (p->intersectsShape(*pt))
    cout << "intersects Point!" << endl;
  Region r;
  pt->getMBR(r);
  cout << "before intersection with MBR" << endl;
  if (p->intersectsShape(r))
    cout << "intersects Region!" << endl;

  geos::geom::Polygon *cp = create_square_polygon(3.5,1.5,5.0);
  GEOSPolygon *gcp = new GEOSPolygon(*cp);
  cout << "GEOSPolygon containment test with Region 1=" << gcp->containsShape(r) << endl;


  geos::geom::Polygon *poly = create_square_polygon(4.0,2.0,3.0);
  GEOSPolygon *gpoly = new GEOSPolygon(*poly);
  cout << "created polygon" << endl;
  if (gpoly->intersectsShape(*p))
    cout << "polygon intersects GEOSPoint!" << endl;
  else
    cout << "polygon does not intersect GEOSPoint!" << endl;
  if (gpoly->containsShape(*p))
    cout << "polygon contains GEOSPoint!" << endl;
  else
    cout << "polygon does not contain GEOSPoint!" << endl;
  if (gpoly->touchesShape(*p))
    cout << "polygon touches GEOSPoint!" << endl;
  else
    cout << "polygon does not touch GEOSPoint!" << endl;

  cout << "point MBR " << r << endl;

  Region polyr;
  gpoly->getMBR(polyr);
  cout << "polygon MBR " << polyr << endl;

  geos::geom::Coordinate c2(3.5, 1.5);
  GEOSPoint *p2 = new GEOSPoint(c2);
  Region r2;
  p2->getMBR(r2);
  cout << "distance " << r2 << " to " << polyr << " = " << p2->getMinimumDistance(*gpoly) << endl;

  SpatialIndex::Point *p3 = new SpatialIndex::Point();
  gpoly->getCenter(*p3);
  cout << "center of " << *gpoly << " is " << *p3 << endl;

  cout << "before store" << endl;

  uint8_t* buffer;
  uint32_t length;
  p2->storeToByteArray(&buffer,length);

  cout << "after store, before load" << endl;

  GEOSPoint *p4 = new GEOSPoint();
  p4->loadFromByteArray(buffer);

  cout << "after load, testing copy" << endl;
  cout << "distance " << p4 << " to " << gpoly << " = " << p4->getMinimumDistance(*gpoly) << endl;

  cout << "testing storage of polygons" << endl;
  gpoly->storeToByteArray(&buffer,length);

  cout << "after store, before load" << endl;

  GEOSPolygon *gpoly2 = new GEOSPolygon();
  gpoly2->loadFromByteArray(buffer);

  cout << "after load, testing copy" << endl;
  cout << "distance " << p4 << " to " << gpoly2 << " = " << p4->getMinimumDistance(*gpoly2) << endl;

  PL_succeed;
}


extern "C" install_t install_space()
{ /* work is done in the static initializers */
}
