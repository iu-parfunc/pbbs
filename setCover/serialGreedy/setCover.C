// This code is part of the Problem Based Benchmark Suite (PBBS)
// Copyright (c) 2012 Guy Blelloch, Kanat Tangwongsan and the PBBS team
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

// This code implements the classical greedy set-cover algorithm.

#include <iostream>
#include <math.h>
#include <vector>
#include "sequence.h"
#include "gettime.h"
#include "graph.h"
#include "parallel.h"
using namespace std;

timer bucketTime;
timer manisTime;
timer packTime;

typedef int intT;
typedef vector<intT> intTvec;
typedef unsigned int uint;
typedef pair<uint, uint> upair;
typedef graph<intT> Graph;

struct set {
  intT* Elements;
  intT degree;
  intT id;
  set(intT* E, intT d, intT i) : Elements(E), degree(d), id(i) {}
};

struct bucket {
  set *S;
  intT n;
  bucket(set* _S, intT _n) : S(_S), n(_n) {}
};

// maximum element ID in any set + 1
intT maxElt(graph<intT> G) {
  intT *C = newA(intT,G.n);
  for (intT i = 0; i < G.n; i++) {
    C[i] = 0;
    for (intT j=0; j < G.V[i].degree; j++) 
      C[i] = max(C[i], G.V[i].Neighbors[j]);
  }
  return sequence::reduce(C, G.n, utils::maxF<intT>()) + 1;
}

void removeSet(intTvec &bin, intT *where, intT loc) 
{
  where[bin[loc]]=-1;
  if (loc != bin.size()-1) {
    bin[loc]=bin[bin.size()-1];
    where[bin[loc]]=loc;
  }
  bin.resize(bin.size()-1);
}

void putInBucket(intT set, intT sz, intTvec **bins, intT *where)
{
  if (bins[sz]==NULL) {
    bins[sz]=new intTvec();
    // cout << "bin sz=" << sz << " created: " << bins[sz] << endl;
  }
  bins[sz]->push_back(set);
  where[set]=bins[sz]->size() - 1;
}

Graph buildInverted(Graph GS, intT m)
{
  intT *tmp=newA(intT, m+1);
  intT *start=newA(intT, m+1);
  for (intT i=0;i<=m;i++) tmp[i]=0;
 
  // count the number of sets containing elt i
  for (intT i=0;i<GS.n;i++)
    for (intT j=0;j<GS.V[i].degree;j++)
      ++tmp[GS.V[i].Neighbors[j]];
  
  intT sum=0;
  for (intT i=0;i<m;sum+=tmp[i],i++) {
    start[i]=sum;
  }
  for (intT i=0;i<m;i++) tmp[i]=0;
  start[m]=sum;

  
  intT *adj=newA(intT, sum);
  for (intT i=0;i<GS.n;i++)
    for (intT j=0;j<GS.V[i].degree;j++) {
      intT elt = GS.V[i].Neighbors[j];
     
      adj[start[elt] + tmp[elt]] = i;
      ++(tmp[elt]);
    }

  vertex<intT>* VN = newA(vertex<intT>, m);
  for (intT i=0;i<m;i++)
    VN[i] = vertex<intT>(adj + start[i], start[i+1] - start[i]);
  Graph GE = Graph(VN, m, sum, adj);
  free(start); free(tmp);
  return GE;
}
_seq<intT> setCover(Graph GS) 
{
  intT m = maxElt(GS);
  intT n = GS.n;
					       
  intT *where = newA(intT, n);
  intT *covered = newA(intT, m);
  intTvec **bins = newA(intTvec *, m+1); 

  intT* inCover = newA(intT, GS.n);
  intT nInCover = 0;
  

  for (intT i=0;i<m;i++) covered[i] = 0;
  for (intT i=0;i<=m;i++) bins[i] = NULL;

  cout << "starting setCover.." << endl;
  for (intT i=0;i<n;i++) putInBucket(i,GS.V[i].degree,bins,where);

  Graph GE = buildInverted(GS, m);
  
  for (intT sz=m;sz>=1;sz--) {
    #ifdef _DEBUG
    if (bins[sz]) {
      cout << "sz: " << sz << ", bucket size: "<< bins[sz]->size() << endl;
      cout << "nInCover: " << nInCover << endl;
    }
    #endif
    while (bins[sz] && bins[sz]->size()>0) {
      intT sid = (*bins[sz])[0];
      removeSet(*(bins[sz]), where, 0);
      // cout << "choose " << sid << endl;
      inCover[nInCover++] = sid;
      
      // knock off what this set covers
      for (intT i=0;i<GS.V[sid].degree;i++) {
	intT elt = GS.V[sid].Neighbors[i];
	if (!covered[elt]) {
	  covered[elt] = true;
	  // update all the sets affected
	  for (intT j=0;j<GE.V[elt].degree;j++) {
	    intT ts = GE.V[elt].Neighbors[j];
	    if (where[ts]>=0 && GS.V[ts].degree>0) {
	      if (bins[GS.V[ts].degree])
		removeSet(*(bins[GS.V[ts].degree]),where, where[ts]);
	      intT d = --GS.V[ts].degree;
	      if (d>0) {
		// cout << "moving " << ts << " to sz=" << d << endl;
		putInBucket(ts,d,bins,where);
	      }
	      else 
		where[ts]=-1; // this set is gone
	    }
	  }
	}
      }
    }
  }

  free(where); free(covered); 
  for (intT i=0;i<m;i++)
    if (bins[i]) {free(bins[i]); bins[i]=NULL;};
  free(bins);
  GE.del();
  cout << "Set cover size: " << nInCover << endl;
  return _seq<intT>(inCover, nInCover);   
}

