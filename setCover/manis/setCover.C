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

// This code is based on the paper:
//    Guy Blelloch, Richard Peng and Kanat Tangwongsan. 
//    Linear-Work Greedy Parallel Approximation Algorithms for Set Covering and Variants. 
//    Proc. ACM Symposium on Parallel Algorithms and Architectures (SPAA), May 2011

#include <iostream>
#include <cassert>
#include <math.h>
#include "sequence.h"
#include "gettime.h"
#include "graph.h"
#include "parallel.h"
#include "blockRadixSort.h"
#include "speculative_for.h"
using namespace std;

typedef int intT;
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
intT maxElt(Graph G) {
  intT *C = newA(intT,G.n);
  parallel_for (intT i = 0; i < G.n; i++) {
    C[i] = 0;
    for (intT j=0; j < G.V[i].degree; j++) 
      C[i] = max(C[i], G.V[i].Neighbors[j]);
  }
  return sequence::reduce(C, G.n, utils::maxF<intT>()) + 1;
}

// Puts each vertex into buckets that are multiples of (1+epsilon) based on degree
std::pair<bucket*,intT> putInBuckets(Graph GS, double epsilon) {
  double x = 1.0/log(1.0 + epsilon);
  intT numBuckets = 1 + floor(x * log((double) GS.n));
  cout << "numBuckets = " << numBuckets << endl;
  intT* offsets = newA(intT, numBuckets + 256);

  // determine bucket numbers
  upair *A = newA(upair, GS.n);
  parallel_for(intT i=0; i < GS.n; i++) {
    //intT d = (GS.V[i].degree > 0) ? GS.V[i].degree : 1;  // Broken
    intT d = GS.V[i].degree;
    // deal with empty buckets
    if (d <=0) d = 1;
    A[i] = upair(floor(x * log((double) d)), i);
  }

  // sort based on bucket numbers
  intSort::iSort(A, offsets, (intT) GS.n, numBuckets, utils::firstF<uint, uint>());
  set *S = newA(set, GS.n);
  parallel_for(intT i=0; i < GS.n; i++) {
    intT j = A[i].second;
    S[i] = set(GS.V[j].Neighbors, GS.V[j].degree, j);
  }
  free(A);

  // create each bucket
  bucket *B = newA(bucket, numBuckets);
  parallel_for(intT i=0; i < numBuckets-1; i++) 
    B[i] = bucket(S+offsets[i],offsets[i+1]-offsets[i]);
  B[numBuckets-1] = bucket(S+offsets[numBuckets-1], GS.n - offsets[numBuckets-1]);
  free(offsets);

  return pair<bucket*,intT>(B, numBuckets);
}

void freeBuckets(bucket* B) {
  free(B[0].S);
  free(B);
}

struct manisStep {
  float lowThreshold, highThreshold;
  intT *elts; set *S;
  manisStep(float lt, float ht, intT* e, set *_S) 
    : lowThreshold(lt), highThreshold(ht), elts(e), S(_S) {}

  bool reserve(intT i) {
    // for set i remove covered elements and count uncovered ones
    intT k = 0;
    for (intT j = 0; j < S[i].degree; j++) {
      intT ngh = S[i].Elements[j];
      if (elts[ngh] > 0) S[i].Elements[k++] = ngh;
    }
    S[i].degree = k;

    // if enough elements remain then reserve them with priority i
    if (k >= highThreshold) {
      for (intT j = 0; j < S[i].degree; j++) 
	utils::writeMin(&elts[S[i].Elements[j]], i);
      return 1;
    } else return 0;
  }

  bool commit(intT i) { 
    // check how many were successfully reserved
    intT k = 0;
    for (intT j = 0; j < S[i].degree; j++) 
      if (elts[S[i].Elements[j]] == i) k++;

    // if enough are reserved, then add set i to the Set Cover
    bool inCover = (k >= lowThreshold);

    // reset reserved elements to -1 if in cover or INT_MAX if not
    for (intT j = 0; j < S[i].degree; j++) 
      if (elts[S[i].Elements[j]] == i) 
	elts[S[i].Elements[j]] = inCover ? -1 : INT_MAX;
    if (inCover) {
      S[i].degree = -1; // marks that vertex is in the cover
      return 1;
    } else return 0;
  }
};

intT manis(set* S, intT* elts, intT n, float lowThreshold, float highThreshold) {
  manisStep manis(lowThreshold, highThreshold, elts, S);
  int NUMBER_OF_ROUNDS = 2;
  return speculative_for(manis, 0, n, NUMBER_OF_ROUNDS, 0);
}

intT serialManis(set* S, intT* elts, intT n, intT threshold, bool* flag, intT* tmp) {
  for (intT i = 0; i < n; i++) {
    intT d = S[i].degree;
    parallel_for (int j = 0; j < d; j++) {
      intT ngh = S[i].Elements[j];
      flag[j] = (elts[ngh] == INT_MAX);
    }
    d = sequence::pack(S[i].Elements, tmp, flag, d);
    parallel_for (int j = 0; j < d; j++) 
      S[i].Elements[j] = tmp[j];

    if (d >= threshold) {
      parallel_for (int j = 0; j < d; j++) 
	elts[S[i].Elements[j]] = -1;
      S[i].degree = -1; // marks that vertex is in set
    } else S[i].degree = d;
  }
  return n;
}

_seq<intT> setCover(Graph GS) {
  double epsilon = 0.01;
  intT m = maxElt(GS);
  cout << "m = " << m << endl;
  timer bucketTime;
  timer manisTime;
  timer packTime;
  bucketTime.start();

  // Convert graph to sets and put sets in buckets based on degree
  pair<bucket*, intT> B = putInBuckets(GS, epsilon);
  bucketTime.stop();

  bucket* allBuckets = B.first;
  intT numBuckets = B.second;

  set* S = newA(set, GS.n);    // holds sets for current bucket
  set* ST = newA(set, GS.n);   // temporarily S (pack is not inplace)
  intT l = 0;                   // size of S
  bool* flagNext = newA(bool, GS.n);
  bool* flagCurrent = newA(bool, GS.n);
  intT* inCover = newA(intT, GS.n);
  intT* tmp = newA(intT, GS.n);
  intT nInCover = 0;
  intT totalWork = 0;
  intT* elts = newA(intT,m);
  intT threshold = GS.n;
  parallel_for (intT i = 0; i < m; i++) elts[i] = INT_MAX;

  // loop over all buckets, largest degree first
  for (intT i = numBuckets-1; i >= 0; i--) {
    bucket currentB = allBuckets[i];

    intT degreeThreshold = ceil(pow(1.0+epsilon,i));
    if (degreeThreshold == threshold && currentB.n == 0) continue;
    else threshold = degreeThreshold;
    intT lowThreshold = ceil(pow(1.0+epsilon,i-1));
    packTime.start();

    // pack leftover sets that are below threshold down for the next round
    //    and sets greater than threshold above for the current round
    parallel_for (intT ii = 0; ii < l; ii++) {
      flagNext[ii] = (S[ii].degree > 0 && S[ii].degree < threshold);
      flagCurrent[ii] = (S[ii].degree >= threshold);
    }    
    intT ln = sequence::pack(S, ST, flagNext, l);
    intT lb = sequence::pack(S, ST+ln, flagCurrent, l);

    // copy prebucketed bucket i to end, also for the current round
    parallel_for (intT j = 0; j < currentB.n; j++) 
      ST[j+ln+lb] = currentB.S[j];

    lb = lb + currentB.n;   // total number in the current round
    l = ln + lb;            // total number including those for next round
    swap(ST,S);             // since pack is not in place 
    set* SB = S + ln;       // pointer to bottom of sets for current round
    packTime.stop();

    if (lb > 0) { // is there anything to do in this round?
      manisTime.start();

      // Find a maximal near independent set such that each set covers at least
      //   threshold independent elements
      intT work;
      if (threshold < 100000) work = manis(SB, elts, lb, lowThreshold, threshold);
      else work = serialManis(SB, elts, lb, threshold, flagCurrent, tmp);
      totalWork += work;
      manisTime.stop();
      //cout << "Manis Time = " << manisTime.stop() << endl;
      packTime.start();

      // check which sets were selected by manis to be in the set cover
      parallel_for (intT j = 0; j < lb; j++)
	flagCurrent[j] = SB[j].degree < 0;

      // add these to inCover and label by their original ID
      intT nNew = sequence::packIndex(inCover+nInCover, flagCurrent, lb);
      parallel_for (intT j = nInCover; j < nInCover + nNew; j++) 
	inCover[j] = SB[inCover[j]].id;
      nInCover = nInCover + nNew;
      packTime.stop();
      //cout << "i = " << i << " bc = " << currentB.n << " l = " << l << " lb = " << lb
      //	   << " work = " << work << " new = " << nNew << " threshold = " << threshold << endl;
    }
  }
  cout << "Set cover size = " << nInCover << endl;
  cout << "Total work = " << totalWork << endl;
  cout << "Bucket Time = " << bucketTime.total() << endl;
  cout << "Manis Time = " << manisTime.total() << endl;
  cout << "Pack Time = " << packTime.total() << endl;

  free(elts); free(S); free(ST); free(flagCurrent); free(flagNext);
  freeBuckets(allBuckets);
  return _seq<intT>(inCover, nInCover); 
}
