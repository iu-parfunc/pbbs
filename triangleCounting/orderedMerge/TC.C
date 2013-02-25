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
#include <cassert>
#include "sequence.h"
#include "graph.h"
#include "utils.h"
#include "parallel.h"

#if defined(CILK) || defined(CILKP)
#include "sampleSort.h"
#elif defined(OPENMP)
#include "stlParallelSort.h"
#else
#include "serialSort.h"
#endif

using namespace std;

// **************************************************************
// * using the degree heuristic to order the vertices
// **************************************************************

struct nodeLT {
  nodeLT(graph<intT> G) : G_(G) {};

  bool operator() (intT a, intT b) { 
    return G_.V[a].degree < G_.V[b].degree;
  };
  
  graph<intT> G_;
};

void rankNodes(graph<intT> G, intT *r, int *o)
{
  parallel_for (intT i=0;i<G.n;i++) o[i] = i;
  compSort(o, G.n, nodeLT(G));
  parallel_for (intT i=0;i<G.n;i++) r[o[i]] = i;  
}

struct isFwd{
  isFwd(intT myrank, intT *r) : r_(r), me_(myrank) {};
  bool operator () (intT v) {return r_[v] > me_;};
  intT me_;
  intT *r_;
};

struct intLT {
  bool operator () (intT a, intT b) { return a < b; };
};

intT countCommon(intT *A, intT nA, intT *B, intT nB)
{
  intT i=0,j=0;
  intT ans=0;
  while (i < nA && j < nB) {
    if (A[i]==B[j]) i++, j++, ans++;
    else if (A[i] < B[j]) i++;
    else j++;
  }
  return ans;
}

struct countFromA {
  
  countFromA (intT *_e, intT *_sz, intT *_start)
    : edges(_e), sz(_sz), start(_start) {} ;

  intT operator() (intT a) {
    intT count = 0;
    for (intT bi=0;bi<sz[a];bi++) {
      intT b=edges[start[a]+bi];
      // assert(rank[b] > rank[a]);
      count += countCommon(edges + start[a], sz[a],
			   edges + start[b], sz[b]);
    }    
    return count;
  }
  intT *edges, *sz, *start;
};

intT countTriangle(graph<intT> G)
{
  intT *rank = newA(intT, G.n);
  intT *order = newA(intT, G.n);

  rankNodes(G, rank, order);

  // create a directed version of G and order the nodes in
  // the increasing order of rank

  intT *edges = newA(intT, G.m);
  intT *sz = newA(intT, G.n);
  intT *start = newA(intT, G.n);
  
  // compute starting point in edges
  parallel_for (intT i=0;i<G.n;i++) sz[i] = G.V[i].degree;
  sequence::plusScan(sz, start, G.n);
  
  parallel_for (intT s=0;s<G.n;s++) {
    sz[s] = sequence::filter(G.V[s].Neighbors, edges + start[s], 
			     G.V[s].degree, isFwd(rank[s], rank));
    compSort(edges + start[s], sz[s], intLT());
  }

  // start counting

  intT count = 0;
  count = sequence::reduce<intT>((intT) 0, (intT) G.n, 
				 utils::addF<intT>(),
				 countFromA(edges,sz,start));
  
  free(edges); free(sz); free(start); free(rank); free(order);

  cout << "tri. count = " << count << endl;
  return count;
}



