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

#ifndef __SEARCH_H
#define __SEARCH_H

#include <SWI-cpp2.h>
#include "globals.h"
#include <spatialindex/SpatialIndex.h>
#include "Index.h"

using namespace std;
using namespace SpatialIndex;


enum RangeQueryType
{
  ContainmentQuery = 0x1,
  IntersectionQuery = 0x2
};


class PrintVisitor : public IVisitor
{
 public:
  void visitNode(const INode& n);
  void visitData(const IData& d);
  void visitData(std::vector<const IData*>& v);
};


class PrintGnuplotVisitor : public IVisitor
{
 public:
  void visitNode(const INode& n);
  void visitData(const IData& d);
  void visitData(std::vector<const IData*>& v);
};


class TraverseBreadthFirst : public SpatialIndex::IQueryStrategy
{
 private:
  queue<id_type> ids;
  IVisitor *v;

 public:
  TraverseBreadthFirst(IVisitor *vis);
  void getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext);
};


class TraverseDepthFirst : public SpatialIndex::IQueryStrategy
{
 private:
  stack<id_type> ids;
  IVisitor *v;

 public:
  TraverseDepthFirst(IVisitor *vis);
  void getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext);
};


class IncrementalRangeStrategy : public SpatialIndex::IQueryStrategy
{
 public:
  atom_t result;
  bool result_found;
  bool continuation;
  uint32_t child_idx;
  const IShape* query;
  IVisitor* v;
  RangeQueryType t;
  stack<id_type> ids;
  Index *index;

  IncrementalRangeStrategy(RangeQueryType type, IShape* queryp,IVisitor* vp,Index* index);
  ~IncrementalRangeStrategy();

  void getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext);
};



class IncrementalNearestNeighborStrategy : public SpatialIndex::IQueryStrategy
{
private:
  class NNEntry
  {
  public:
    id_type m_id;
    IEntry* m_pEntry;
    double m_minDist;

    NNEntry(id_type id, IEntry* e, double f) : m_id(id), m_pEntry(e), m_minDist(f) {}
    ~NNEntry() {
      if (m_pEntry != NULL)
        delete m_pEntry;
    }

    struct ascending
    {
      bool operator()(const NNEntry* __x, const NNEntry* __y) const { return __x->m_minDist > __y->m_minDist; }
    };
  }; // NNEntry

  class NNComparator : public INearestNeighborComparator
  {
  public:
    double getMinimumDistance(const IShape& query, const IShape& entry)
    {
      return query.getMinimumDistance(entry);
    }

    double getMinimumDistance(const IShape& query, const IData& data)
    {
      IShape* pS;
      data.getShape(&pS);
      double ret = query.getMinimumDistance(*pS);
      delete pS;
      return ret;
    }
  }; // NNComparator

 public:
  atom_t result;
  bool result_found;
  bool continuation;
  uint32_t child_idx;
  bool first_call;
  const IShape* query;
  IVisitor* v;
  std::priority_queue<NNEntry*, std::vector<NNEntry*>, NNEntry::ascending> queue;
  NNComparator nnc;
  Index *index;

  IncrementalNearestNeighborStrategy( IShape* queryp,IVisitor* vp, Index* index);
  void getNextEntry(const IEntry& entry, id_type& nextEntry, bool& hasNext);
  ~IncrementalNearestNeighborStrategy();
};

#endif // __SEARCH_H
