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

#include "utils.h"
#include "sequence.h"
#include "graph.h"
#include "parallel.h"
using namespace std;

// **************************************************************
//    HYBRID BREADTH FIRST SEARCH
// **************************************************************

// **************************************************************
//    Switches to Dense matrix-vector multiply when frontier size
//    is large enough
// **************************************************************

struct nonNegF{bool operator() (intT a) {return (a>=0);}};

pair<intT,intT> BFS(intT start, graph<intT> GA) {
  intT numVertices = GA.n;
  intT numEdges = GA.m;
  vertex<intT> *G = GA.V;
  intT* Frontier = newA(intT,numEdges);
  intT* Visited = newA(intT,numVertices);
  intT* NextVisited = newA(intT,numVertices);
  intT* FrontierNext = newA(intT,numEdges);
  intT* Counts = newA(intT,numVertices);
  {parallel_for(intT i = 0; i < numVertices; i++) {
      Visited[i] = 0;
      NextVisited[i]=0;
    }
  }
  Frontier[0] = start;
  intT frontierSize = 1;
  Visited[start] = 1;

  intT totalVisited = 0;
  int round = 0;
  int threshold = numEdges/50;
  bool fromSMV = 1;
  while (frontierSize > 0) {
    round++;
    if(frontierSize < threshold){
      cout<<"round "<<round<<" frontierSize="<<frontierSize<<" sparse-sparse method\n";
      //totalVisited += frontierSize;
      if(fromSMV){
	fromSMV = 0;
	parallel_for(intT i=0;i<numVertices;i++) Counts[i] = 0;
	
	parallel_for(intT i=0;i<numVertices;i++)
	  if(NextVisited[i] != Visited[i]) Counts[i] = 1;
	intT x = sequence::scan(Counts,Counts,numVertices,utils::addF<intT>(),(intT)0);
	//assert(x == frontierSize);
	parallel_for(intT i=0;i<numVertices-1;i++){
	  if(Counts[i] != Counts[i+1]) Frontier[Counts[i]] = i;
	}
	if(Counts[numVertices-1] == frontierSize-1)
	  Frontier[frontierSize-1] = numVertices-1;
	// for(intT i=0;i<numVertices;i++){
	//   if(NextVisited[i] != Visited[i])
	//     Frontier[frontierSize++]=i;
	// }
      }
      {parallel_for (intT i=0; i < frontierSize; i++) 
	  Counts[i] = G[Frontier[i]].degree;}
      intT nr = sequence::scan(Counts,Counts,frontierSize,utils::addF<intT>(),(intT)0);

      // For each vertexB in the frontier try to "hook" unvisited neighbors.
      {parallel_for(intT i = 0; i < frontierSize; i++) {
	  intT k= 0;
	  intT v = Frontier[i];
	  intT o = Counts[i];
	  for (intT j=0; j < G[v].degree; j++) {
	    intT ngh = G[v].Neighbors[j];
	    if (Visited[ngh] == 0 && utils::CAS(&Visited[ngh],(intT)0,(intT)1)) 
	      {
		FrontierNext[o+j] = ngh;//G[v].Neighbors[k++] = ngh;
	      }
	    else FrontierNext[o+j] = -1;
	  }
	  G[v].degree = k;
	}}

      // Filter out the empty slots (marked with -1)
      frontierSize = sequence::filter(FrontierNext,Frontier,nr,nonNegF());
    }
    else {
      //spsv
      //threshold = 0;
      cout<<"round "<<round<<" frontierSize="<<frontierSize<<" sparse-dense method\n";
      parallel_for(intT i=0;i<numVertices;i++){
	NextVisited[i] = Visited[i];
	if(!NextVisited[i]){
	  for(intT j=0;j<G[i].degree;j++){
	    intT ngh = G[i].Neighbors[j];
	    if(Visited[ngh]) {
	      NextVisited[i] = 1;
	      break;
	    }
	  }
	}
      }
      parallel_for(intT i=0;i<numVertices;i++) Counts[i] = 0;
      
      parallel_for(intT i=0;i<numVertices;i++)
	if(NextVisited[i] != Visited[i])
	  Counts[i]=1;
      frontierSize = sequence::plusScan(Counts,Counts,numVertices);
      if(frontierSize < threshold) fromSMV=1;
      swap(NextVisited,Visited);
    }
  }
  free(FrontierNext); free(Frontier); free(Counts); free(Visited);
  free(NextVisited);
  return pair<intT,intT>(totalVisited,round);
}


