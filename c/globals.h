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

#ifndef __GLOBALS_H
#define __GLOBALS_H

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif

#include <SWI-cpp2.h>
#define USE_UNSTABLE_GEOS_CPP_API 1
#include <geos/geom/GeometryFactory.h>

// Geometry
static PlAtom ATOM_point("point");
static PlAtom ATOM_box("box");
static PlAtom ATOM_linestring("linestring");
static PlAtom ATOM_linearring("linearring");
static PlAtom ATOM_polygon("polygon");

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

extern geos::geom::GeometryFactory::Ptr global_factory;

#endif // __GLOBALS_H
