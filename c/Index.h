/*  Part of SWI-Prolog

    Author:        Willem Robert van Hage
    E-mail:        wrvhage@few.vu.nl
    WWW:           http://www.few.vu.nl/~wrvhage
    Copyright (c)  2009-2011, Vrije Universiteit Amsterdam
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

#ifndef __INDEX_H
#define __INDEX_H

#include <SWI-cpp2.h>
#include "globals.h"
#include "Shapes.h"
#include "lock.h"


typedef enum {
  PYTHAGOREAN,
  HAVERSINE
} distance_function_t;

typedef enum {
  MEMORY,
  DISK
} storage_t;

class IteratorState {
  public:
    pair<multimap<atom_t,id_type>::iterator,
         multimap<atom_t,id_type>::iterator> uri_id_range;
    multimap<atom_t,id_type>::iterator uri_id_iter;
};

using namespace std;
using namespace SpatialIndex;


class Index
{
 public:
  virtual ~Index() {};

  virtual id_type get_new_id(PlTerm uri) = 0;
  virtual void storeShape(id_type id,IShape *s,PlTerm shape_term) = 0;
  virtual IShape* getShape(id_type id) = 0;
  virtual bool getShapeTerm(id_type id,term_t t) = 0;
  virtual void deleteShape(id_type id) = 0;

  virtual IShape* interpret_shape(PlTerm shape_term) = 0;
  virtual void clear_tree() = 0;
  virtual void create_tree(uint32_t dimensionality) = 0;
  virtual void create_tree(uint32_t dimensionality, double util, int nodesz) = 0;
  virtual bool insert_single_object(PlTerm uri,PlTerm shape_term) = 0;
  virtual bool delete_single_object(PlTerm uri,PlTerm shape_term) = 0;

};


class RTreeIndex : public Index
{
 public:
  storage_t storage;
  distance_function_t distance_function;

  PlAtom baseName;
  double utilization;
  int nodesize;
  IStorageManager* storage_manager;
  StorageManager::IBuffer* buffer;
  ISpatialIndex* tree;
  id_type indexIdentifier;
  multimap<atom_t,id_type> uri_id_multimap;
  map<id_type,pair<IShape*,record_t> > id_shape_map;

  RTreeIndex(PlTerm indexname);
  RTreeIndex(PlTerm indexname, double util, int nodesz);
  virtual ~RTreeIndex() override;

  virtual id_type get_new_id(PlTerm uri) override;
  virtual void storeShape(id_type id,IShape *s,PlTerm t) override;
  virtual IShape* getShape(id_type id) override;
  virtual bool getShapeTerm(id_type id,term_t t) override;
  virtual void deleteShape(id_type id) override;

  virtual IShape* interpret_shape(PlTerm shape_term) override;
  virtual bool bulk_load(PlTerm goal,uint32_t dimensionality);
  virtual void clear_tree() override;
  virtual void create_tree(uint32_t dimensionality) override;
  virtual void create_tree(uint32_t dimensionality, double util, int nodesz) override;
  virtual bool insert_single_object(PlTerm uri,PlTerm shape_term) override;
  virtual bool delete_single_object(PlTerm uri,PlTerm shape_term) override;
  
 public:
  id_type bulkload_tmp_id_cnt;

 public:
  rwlock lock;


};

#endif // __INDEX_H
