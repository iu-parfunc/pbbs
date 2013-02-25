// This code is part of the Problem Based Benchmark Suite (PBBS)
// Copyright (c) 2011 Guy Blelloch and the PBBS team
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights (to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#include <iostream>
#include <algorithm>
#include <set>
#include <cstring>
#include "graph.h"
#include "utils.h"
#include "parallel.h"
using namespace std;

// **************************************************************
// * serial basic triangle counting...
// **************************************************************

typedef set<intT> intSetT;

intT countIntersect(intSetT A, intSetT B)
{
  if (B.size() < A.size()) return countIntersect(B, A);

  int count=0;
  for (intSetT::iterator it=A.begin();it!=A.end();it++)
    count += B.count(*it);


  return count;
}
intT naiveCount(graph<intT> G)
{
  intSetT *nbrs;
  intT count;
  // building an auxiliary data structure nbrs, where
  // nbrs[i] is a set of neighbors with id > i
  nbrs = new intSetT[G.n];
  for (intT i=0;i<G.n;i++) {
    for (intT dst=0;dst<G.V[i].degree;dst++) {
      intT t=G.V[i].Neighbors[dst];
      if (t>i) nbrs[i].insert(t);
    }
  }

  count=0;
  for (intT a=0;a<G.n-1;a++){
    for (intT bi=0;bi<G.V[a].degree;bi++) {
      intT b=G.V[a].Neighbors[bi];
      if (b > a) count += countIntersect(nbrs[a], nbrs[b]);
    }
  }
  delete [] nbrs;

  return count;
}

intT countTriangle(graph<intT> G)
{
  return naiveCount(G);
}



