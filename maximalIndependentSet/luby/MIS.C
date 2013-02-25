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
#include "sequence.h"
#include "graph.h"
#include "parallel.h"
#include <algorithm>
using namespace std;

// **************************************************************
//    MAXIMAL INDEPENDENT SET
// **************************************************************

// For each vertex:
//   Flags = 0 indicates undecided
//   Flags = 1 indicates chosen
//   Flags = 2 indicates a neighbor is chosen

#define xPACK_EDGES

void MIS(vertex<intT>* G, intT* vertices, char* Flags,
	 intT* hold, intT* priorities, bool* keep, char* fate, intT n){
  intT round = 0;
  intT numRemain = n;
  while(numRemain > 0){

    parallel_for(intT i=0;i<numRemain;i++){
      priorities[vertices[i]] = utils::hash((i+round+numRemain))%(10*n);
      fate[i] = 1;
    }
    parallel_for(intT i=0;i<numRemain;i++){
      intT v = vertices[i];
      intT d = G[v].degree;
      intT pr = priorities[v];
      intT k = 0;
      for(intT j=0;j<d;j++){
	intT ngh = G[v].Neighbors[j];
	intT ngh_pr = priorities[ngh];
	if(Flags[ngh] == 1) { fate[i] = 2; keep[i] = 0; break; } //not in set
  
	else if(Flags[ngh] == 0){
	  if(ngh_pr > pr || ((ngh_pr == pr) && (v > ngh))) { //tbd
	    fate[i] = 0; keep[i] = 1;
#ifndef PACK_EDGES
	    break;
#endif
	  }
#ifdef PACK_EDGES
	  G[v].Neighbors[k++] = ngh;
#endif
	}
	
      }
#ifdef PACK_EDGES
      G[v].degree = k;
#endif
    }
        
    parallel_for(intT i=0;i<numRemain;i++){
      intT v = vertices[i];
      char destiny = fate[i];
      if(destiny == 1) { Flags[v] = 1; keep[i] = 0; }
      else if(destiny == 2) { Flags[v] = 2; }
    }

    numRemain = sequence::pack(vertices,hold,keep,numRemain);
    swap(vertices,hold);
    round++;
  }
  
}

char* maximalIndependentSet(graph<intT> GS) {
  intT n = GS.n;
  vertex<intT>* G = GS.V;

  intT* vertices = newA(intT,n);
  parallel_for(intT i=0;i<n;i++) vertices[i] = i;
  char* Flags = newArray(n, (char) 0);
  intT* hold = newA(intT,n);
  intT* priorities = newA(intT,n);
  bool* keep = newA(bool,n);
  char* fate = newA(char,n);
  MIS(G, vertices, Flags, hold, priorities, keep, fate, n);

  free(vertices); free(hold); free(priorities); free(keep);
  free(fate);
  return Flags;
}
