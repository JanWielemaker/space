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

/*
 * TODO: improve error message handling
 * TODO: add uri_shape(U,S,Source) from index
 */

#include <config.h>
#include <utility>
#include <functional>
#include "Index.h"


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


/*
 * -- DataStream --------------------------------------------
 */

class RTreePrologStream : public IDataStream
{
public:
  qid_t q; // TODO: PlQuery
  predicate_t p; // TODO: PlPredicate
  term_t cache0; // TODO: PlTermv
  bool cached;
  RTreeIndex *index;
public:
  RTreePrologStream(term_t goal,RTreeIndex* i) {
    cache0 = Plx_new_term_refs(3);
    p = Plx_predicate("call",3,"system");
    cached = false;
    Plx_put_term(cache0+0,goal);
    q = Plx_open_query(NULL, PL_Q_NORMAL, p, cache0);
    index = i;
    index->bulkload_tmp_id_cnt = 0;
  }
  ~RTreePrologStream() {
    Plx_cut_query(q);
  }
  virtual IData* getNext() {
    RTree::Data* rv = NULL;
    if ( !WRLOCK(&index->lock, FALSE) ) {
      cerr << __FUNCTION__ << " could not acquire write lock" << endl;
      return NULL;
    }
    if (!cached && !PL_next_solution(q)) {
      rv = NULL;
    } else {
      cached = false;
      term_t shape_term = Plx_new_term_ref();
      shape_term = (term_t)(cache0+2);
      IShape *s = index->interpret_shape(PlTerm(shape_term));
      Region r;
      s->getMBR(r);
      PlTerm uri_term((term_t)cache0+1);
      PlAtom uri_atom(uri_term.as_atom());
      id_type id = index->get_new_id(uri_term);
      index->storeShape(id,s,PlTerm(shape_term));
#ifdef DEBUGGING
      cout << "uri " << (char*)uri_term << " atom " << uri_atom.handle << " shape " << r << " id " << id << endl;
#endif
      rv = new RTree::Data(sizeof(uri_atom.unwrap()), reinterpret_cast<uint8_t*>(uri_atom.unwrap_as_ptr()), r, id);
    }
    WRUNLOCK(&index->lock);
    return rv;
  }
  virtual bool hasNext()
  {
    if (cached) return true;
    if (PL_next_solution(q)) {
      cached = true;
      return true;
    }
    cached = false;
    return false;
  }

  virtual uint32_t size()
  {
    throw Tools::NotSupportedException("Operation not supported.");
    return 0;
  }

  virtual void rewind()
  {
    cerr << "rewinding a RTreePrologStream does nothing" << endl;
  }
};



/*
 * -- RTreeIndex -------------------------------------
 */


RTreeIndex::RTreeIndex(PlTerm indexname) :  storage(MEMORY), distance_function(PYTHAGOREAN), baseName(indexname.as_atom()), utilization(0.7), nodesize(4),  storage_manager(NULL), buffer(NULL), tree(NULL) {
  bulkload_tmp_id_cnt = -1;
  INIT_LOCK(&lock);
  indexname.as_atom().register_ref();
}

RTreeIndex::RTreeIndex(PlTerm indexname, double util, int nodesz) : storage(MEMORY), distance_function(PYTHAGOREAN), baseName(indexname.as_atom()), storage_manager(NULL), buffer(NULL), tree(NULL) {
  utilization = util;
  nodesize = nodesz;
  bulkload_tmp_id_cnt = -1;
  indexname.as_atom().register_ref();
  INIT_LOCK(&lock);
}

RTreeIndex::~RTreeIndex() {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  this->clear_tree();
  baseName.unregister_ref();
  WRUNLOCK(&lock);
}

void RTreeIndex::clear_tree() {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  delete tree;
  tree = nullptr;
  delete buffer;
  buffer = nullptr;
  delete storage_manager;
  storage_manager = nullptr;
  uri_id_multimap.clear();
  for( auto& id_shape_iter : id_shape_map ) {
    GEOSShape *s = dynamic_cast<GEOSShape*>(id_shape_iter.second.first);
    if (s != NULL) {
      global_factory->destroyGeometry(s->g);
      s->g = NULL;
    }
    delete id_shape_iter.second.first;
    Plx_erase(id_shape_iter.second.second);
  }
  id_shape_map.clear();
  WRUNLOCK(&lock);
}

void RTreeIndex::storeShape(id_type id,IShape *s,PlTerm t) {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  id_shape_map[id] = make_pair(s,Plx_record(t.unwrap()));
  WRUNLOCK(&lock);
}

IShape* RTreeIndex::getShape(id_type id) {
  IShape *rv = NULL;
  if ( !RDLOCK(&lock) ) {
    cerr << __FUNCTION__ << " could not acquire read lock" << endl;
    return NULL;
  }
  auto iter = id_shape_map.find(id);
  if (iter == id_shape_map.end()) {
    rv = NULL;
  } else {
    rv = iter->second.first;
  }
  RDUNLOCK(&lock);
  return rv;
}

// FIXME: obsolete function, replaced by code in delete_single_object
void RTreeIndex::deleteShape(id_type id) {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire read lock" << endl;
    return;
  }
  auto shape_iter = id_shape_map.find(id);
  if (shape_iter != id_shape_map.end()) {
    GEOSShape *s = dynamic_cast<GEOSShape*>(shape_iter->second.first);
    if (s != NULL) {
      global_factory->destroyGeometry(s->g);
      s->g = NULL;
    }
    delete shape_iter->second.first;
    Plx_erase(shape_iter->second.second);
  }
  id_shape_map.erase(id);
  WRUNLOCK(&lock);
}

bool RTreeIndex::getShapeTerm(id_type id, PlTerm t) {
  bool rv = false;
  if ( !RDLOCK(&lock) ) {
    cerr << __FUNCTION__ << " could not acquire read lock" << endl;
    rv = false;
  }
  auto iter = id_shape_map.find(id);
  if (iter == id_shape_map.end()) {
    rv = false;
  } else {
    rv = PL_recorded(iter->second.second,t.unwrap());
  }
  RDUNLOCK(&lock);
  return rv;
}

id_type  RTreeIndex::get_new_id(PlTerm uri) {
  id_type id = -1;
  if ( !WRLOCK(&lock, FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return (id_type)0;
  }
  PlAtom uri_atom(uri.as_atom());
  uri_atom.register_ref(); // FIXME: unregister somewhere...
  if (bulkload_tmp_id_cnt != -1) { // we're bulkloading
    id = bulkload_tmp_id_cnt++;
    uri_id_multimap.insert(make_pair(uri_atom.unwrap(),id));
  } else {
    if (tree == NULL) {
      id = -1;
    } else {
      IStatistics* stats;
      tree->getStatistics(&stats);
      id = stats->getNumberOfData();
      uri_id_multimap.insert(make_pair(uri_atom.unwrap(),id));
      delete stats;
    }
  }
  WRUNLOCK(&lock);
  return id;
}

void RTreeIndex::create_tree(uint32_t dimensionality) {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  RTreeIndex::create_tree(dimensionality,utilization,nodesize);
  WRUNLOCK(&lock);
}
void RTreeIndex::create_tree(uint32_t dimensionality, double util, int nodesz) {
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return;
  }
  if (tree != NULL) clear_tree();
  utilization = util;
  nodesize = nodesz;
  PlTerm bnt(baseName);
  if (storage == MEMORY) {
    storage_manager = StorageManager::createNewMemoryStorageManager();
  } else if (storage == DISK) {
    string bns = bnt.as_string();
    storage_manager = StorageManager::createNewDiskStorageManager(bns, 32);
  }
  buffer = StorageManager::createNewRandomEvictionsBuffer(*storage_manager, 4096, false);
  tree = RTree::createNewRTree(*buffer, utilization,
                               nodesize, nodesize, dimensionality,
                               SpatialIndex::RTree::RV_RSTAR, indexIdentifier);
  WRUNLOCK(&lock);
}


bool
RTreeIndex::bulk_load(PlTerm goal,uint32_t dimensionality) {
  if (dimensionality > 3 || dimensionality < 1) {
    cerr << "only dimensionality from 1 to 3 supported, not " << dimensionality << endl;
    return false;
  }
  RTreePrologStream stream(goal.unwrap(),this); // assuming goal of the form 'somepred(URI,Shape)'

  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return false;
  }
  if (tree != NULL) clear_tree();
  if (storage == MEMORY) {
    storage_manager = StorageManager::createNewMemoryStorageManager();
  } else if (storage == DISK) {
    string bns = baseName.as_string();
    storage_manager = StorageManager::createNewDiskStorageManager(bns, 32);
  }
  buffer = StorageManager::createNewRandomEvictionsBuffer(*storage_manager, 4096, false);
  id_type indexIdentifier;
  tree = RTree::createAndBulkLoadNewRTree(RTree::BLM_STR,
                                          stream, *buffer, utilization,
                                          nodesize, nodesize, dimensionality,
                                          SpatialIndex::RTree::RV_RSTAR, indexIdentifier);
  WRUNLOCK(&lock);
  if ( !RDLOCK(&lock) ) {
    return FALSE;
  }
  bool rv = tree->isIndexValid();
  RDUNLOCK(&lock);
  return rv;
}

IShape* RTreeIndex::interpret_shape(PlTerm shape_term) {
  static PlAtom ATOM_point("point");
  static PlAtom ATOM_box("box");
  static PlAtom ATOM_linestring("linestring");
  static PlAtom ATOM_linearring("linearring");
  static PlAtom ATOM_polygon("polygon");

  if (shape_term.name() == ATOM_point) {

    double *point = new double[shape_term.arity()];
    for (size_t i = 1; i <= shape_term.arity(); i++) {
      point[i-1] = shape_term[i].as_double();
    }
    GEOSPoint *p = new GEOSPoint(point,shape_term.arity()); // testing GEOS points
    #ifdef DEBUGGING
    cout << "made point " << p << endl;
    #endif
    return p;


  } else if (shape_term.name() == ATOM_linestring) {

    if (shape_term.arity() != 1) {
      cout << "arity not 1: linestring must have one argument, a list containing a list of points" << endl;
      return NULL;
    } else if (!shape_term[1].is_list()) {
      cout << "first argument not a list: linestring must have one argument, a list containing a list of points" << endl;
      return NULL;
    }
    auto cl = make_unique<geos::geom::CoordinateArraySequence>();
    PlTerm_tail list(shape_term[1]);
    PlTerm_var pt;
    while (list.next(pt)) {
      if (pt.name() != ATOM_point) {
        cerr << "linestring contains non-point" << endl;
        return NULL;
      }
      uint32_t dim = pt.arity();
      if (dim == 2) {
        cl->add(geos::geom::Coordinate(pt[1].as_double(),pt[2].as_double()));
      } else if (dim == 1) {
        cl->add(geos::geom::Coordinate(pt[1].as_double(), 0));
      } else if (dim == 3) {
        cl->add(geos::geom::Coordinate(pt[1].as_double(),pt[2].as_double(),pt[3].as_double()));
      } else {
        cerr << dim << " dimensional points not supported" << endl;
      }
    }
    unique_ptr<geos::geom::LineString> ls(global_factory->createLineString(*cl));
    GEOSLineString *linestring = new GEOSLineString(*ls);
    return linestring;

  } else if (shape_term.name() == ATOM_polygon) {

    if (shape_term.arity() != 1) {
      cout << "arity not 1: polygon must have one argument, a list containing a list of points representing the shell, and an second argument, a list containing lists of points representing the holes" << endl;
      return NULL;
    } else if (!shape_term[1].is_list()) {
      cout << "first argument not a list: polygon must have one argument, a list containing a list of points representing the shell, and an second argument, a list containing lists of points representing the holes" << endl;
      return NULL;
    }
    auto cl = make_unique<geos::geom::CoordinateArraySequence>();
    PlTerm_tail linearrings(shape_term[1]);
    PlTerm_var ring;
    PlCheckFail(linearrings.next(ring)); // TODO(peter): throw exception?
    PlTerm_tail shell(ring);
    PlTerm_var pt;
    while (shell.next(pt)) {
      if (pt.name() != ATOM_point) {
        cerr << "polygon shell contains non-point" << endl;
        return NULL;
      }
      uint32_t dim = pt.arity();
      if (dim == 2) {
        cl->add(geos::geom::Coordinate(pt[1].as_double(),pt[2].as_double()));
      } else if (dim == 1) {
        cl->add(geos::geom::Coordinate(pt[1].as_double(), 0));
      } else if (dim == 3) {
        cl->add(geos::geom::Coordinate(pt[1].as_double(),pt[2].as_double(),pt[3].as_double()));
      } else {
        cerr << dim << " dimensional points not supported" << endl;
      }
    }
    // assuming linear ring is already closed
    //    cl->add(cl->getAt(0));
    geos::geom::LinearRing *lr = global_factory->createLinearRing(*cl);
    vector<geos::geom::LinearRing*> *holes = new vector<geos::geom::LinearRing*>;
    while (linearrings.next(ring)) {
      geos::geom::CoordinateArraySequence *hcl = new geos::geom::CoordinateArraySequence();
      PlTerm_tail hole(ring);
      PlTerm_var hpt;
      while (hole.next(hpt)) {
        if (hpt.name() != ATOM_point) {
          cerr << "polygon hole contains non-point" << endl;
          return NULL;
        }
        uint32_t dim = hpt.arity();
        if (dim == 2) {
          hcl->add(geos::geom::Coordinate(hpt[1].as_double(),hpt[2].as_double()));
        } else if (dim == 1) {
          hcl->add(geos::geom::Coordinate(hpt[1].as_double(), 0));
        } else if (dim == 3) {
          hcl->add(geos::geom::Coordinate(hpt[1].as_double(),hpt[2].as_double(),hpt[3].as_double()));
        } else {
          cerr << dim << " dimensional points not supported" << endl;
        }
      }
      // assuming linear ring is already closed
      //      hcl->add(cl->getAt(0));
      geos::geom::LinearRing *hlr = global_factory->createLinearRing(hcl);
      holes->push_back(hlr);
    }
    unique_ptr<geos::geom::Polygon> p(global_factory->createPolygon(lr,holes));
    GEOSPolygon *poly = new GEOSPolygon(*p);
    return poly;

  } else if (shape_term.name() == ATOM_box) {

    #ifdef DEBUGGING
    cout << "reading box" << endl;
    #endif
    if (shape_term.arity() != 2) {
      cerr << "MBR dimensionality must be 2 (low and high), but is " << shape_term.arity() << endl;
      return NULL;
    }
    int dim = shape_term.arity();
    double *low_point = new double[dim];
    double *high_point = new double[dim];
    if (shape_term[1].name() != ATOM_point) return NULL;
    for (int i = 1; i <= dim; i++) {
      low_point[i-1] = shape_term[1][i].as_double();
    }
    if (shape_term[2].name() != ATOM_point) return NULL;
    for (int i = 1; i <= dim; i++) {
      high_point[i-1] = shape_term[2][i].as_double();
    }

    geos::geom::Polygon *box;
    if (dim == 2) {
      auto cl = make_unique<geos::geom::CoordinateArraySequence>();
      cl->add(geos::geom::Coordinate(low_point[0], low_point[1]));
      cl->add(geos::geom::Coordinate(low_point[0], high_point[1]));
      cl->add(geos::geom::Coordinate(high_point[0], high_point[1]));
      cl->add(geos::geom::Coordinate(high_point[0], low_point[1]));
      cl->add(geos::geom::Coordinate(low_point[0], low_point[1]));
      geos::geom::LinearRing *lr = global_factory->createLinearRing(*cl);
      box = global_factory->createPolygon(lr, NULL);
      box->normalize();
    } else if (dim == 1) {
      auto cl = make_unique<geos::geom::CoordinateArraySequence>();
      cl->add(geos::geom::Coordinate(low_point[0], 0));
      cl->add(geos::geom::Coordinate(high_point[0], 0));
      geos::geom::LinearRing *lr = global_factory->createLinearRing(*cl);
      box = global_factory->createPolygon(lr, NULL);
      box->normalize();
    } else if (dim == 3) {
      cerr << "3d box regions not implemented yet" << endl;
      return NULL;
    } else {
      cerr << dim << " dimensional regions not supported" << endl;
      return NULL;
    }
    GEOSPolygon *poly = new GEOSPolygon(*box);
    global_factory->destroyGeometry(box);
    return poly;
  } else {
    cerr << "shape type \"" << shape_term.name().as_string() << "\" unsupported" << endl;
    return NULL;
  }
  return NULL;
}

bool RTreeIndex::insert_single_object(PlTerm uri,PlTerm shape_term) {
  IShape *shape;
  id_type id;
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return FALSE;
  }
  if ((shape = RTreeIndex::interpret_shape(shape_term)) == NULL) {
    cerr << "could not interpret shape" << endl;
    WRUNLOCK(&lock);
    return FALSE;
  }
  if (tree == NULL) {
    uint32_t dimensionality = shape->getDimension();
    clear_tree();
    RTreeIndex::create_tree(dimensionality,utilization,nodesize);
  }
  if ((id = get_new_id(uri)) == -1) {
    cerr << "could not generate new ID" << endl;
    WRUNLOCK(&lock);
    return FALSE;
  }
  try {
    PlAtom uri_atom(uri.as_atom());
    tree->insertData(sizeof(uri_atom.unwrap()), reinterpret_cast<uint8_t*>(uri_atom.unwrap_as_ptr()), *shape, id);
  } catch (...) {
    WRUNLOCK(&lock);
    return FALSE;
  }
  storeShape(id,shape,shape_term);
  WRUNLOCK(&lock);
  return TRUE;
}

bool RTreeIndex::delete_single_object(PlTerm uri,PlTerm shape_term) {
  bool rv = TRUE;
  if (tree == NULL) {
    return TRUE;
  }
  if ( !WRLOCK(&lock,FALSE) ) {
    cerr << __FUNCTION__ << " could not acquire write lock" << endl;
    return FALSE;
  }
  auto state = make_unique<IteratorState>();
  state->uri_id_range = uri_id_multimap.equal_range(uri.as_atom().unwrap());
  for ( state->uri_id_iter = state->uri_id_range.first;
        state->uri_id_iter != state->uri_id_range.second;
        ++(state->uri_id_iter)) {
    id_type id = state->uri_id_iter->second;
    auto id_shape_iter = id_shape_map.find(id);
    if (id_shape_iter != id_shape_map.end()) {
      term_t t = Plx_new_term_ref();
      if (!PL_recorded(id_shape_iter->second.second,t)) {
        cerr << __FUNCTION__ << " couldn't resolve shape term record" << endl;
        return FALSE;
      }
      PlTerm st = PlTerm(t);
      if (st == shape_term) {
        // FIXME: improve error handling. rv should be dependent on all frees
        uri_id_multimap.erase(state->uri_id_iter);
	IShape *shape = (*id_shape_iter).second.first;
	rv = tree->deleteData(*shape, id);
        //deleteShape(id);
        GEOSShape *s = dynamic_cast<GEOSShape*>(shape);
        if (s != NULL) {
          global_factory->destroyGeometry(s->g);
          s->g = NULL;
        }
        Plx_erase(id_shape_iter->second.second);
        id_shape_map.erase(id);
        break;
      }
    }
  }
  WRUNLOCK(&lock);
  return rv;
}
