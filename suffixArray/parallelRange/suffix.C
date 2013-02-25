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
#include "gettime.h"
#include "sequence.h"
#include "blockRadixSort.h"
#include "quickSort.h"
#include "parallel.h"
#include "SA.h"
using namespace std;

#define printInfo

#ifdef printInfo
#define nextTimeM(_str) nextTime(_str)
#else
#define nextTimeM(_str) 
#endif

typedef unsigned int uint;
typedef unsigned char uchar;
typedef pair<uintT,uintT> intpair;

struct seg {
  intT start;
  intT length;
  seg(intT s, intT l) : start(s), length(l) {}
};

struct isSeg {bool operator() (seg s) {return s.length > 1;}};

inline intT grabChars(uchar *s, int bits, intT nChars) {
  intT r = s[0];
  for (intT i=1; i < nChars; i++) r = r<<bits | s[i];
  return r; 
}

inline intT grabCharsEnd(uchar *s, int bits, intT nChars, intT end) {
  intT r = s[0];
  for (intT i=1; i < nChars; i++) 
    r = r<<bits | ((i < end) ? s[i] : 0);
  return r; 
}

struct pairCompF {
  bool operator() (intpair A, intpair B) { return A.first < B.first;}};

void splitSegment(seg *segOut, intT start, intT l, intT* ranks, intpair *Cs,
		  bool addRanks) {
  if (l < 1000) { // sequential version

    if (addRanks) {
      // if following two loops are fused performance goes way down?
      intT name = 0;
      ranks[Cs[0].second] = name + start + 1;
      for (intT i=1; i < l; i++) {
	if (Cs[i-1].first != Cs[i].first) name = i;
	ranks[Cs[i].second] = name + start + 1;
      }
    }

    intT name = 0;
    for (intT i=1; i < l; i++) {
      if (Cs[i-1].first != Cs[i].first) {
	segOut[i-1] = seg(name+start,i-name);
	name = i;
      } else segOut[i-1] = seg(0,0);
    }
    segOut[l-1] = seg(name+start,l-name);

  } else { // parallel version
    intT *names = newA(intT,l);

    parallel_for (intT i = 1;  i < l;  i++) 
      names[i] = (Cs[i].first != Cs[i-1].first) ? i : 0;
    names[0] = 0;
    //nextTimeM("names");

    sequence::scanI(names,names,l,utils::maxF<intT>(),(intT)0);
    //nextTimeM("scan");

    if (addRanks) 
      parallel_for (intT i = 0;  i < l;  i++) 
	ranks[Cs[i].second] = names[i]+start+1;
    //nextTimeM("scatter");

    parallel_for (intT i = 1;  i < l;  i++)
      if (names[i] == i) 
	segOut[i-1] = seg(start+names[i-1],i-names[i-1]);
      else segOut[i-1] = seg(0,0);
    segOut[l-1] = seg(start+names[l-1],l-names[l-1]);
    //nextTimeM("segout");

    free(names);
  }
}  

void brokenCilk(intT nSegs, seg *segments, intpair *C, intT offset, intT n, intT* ranks, seg *segOuts, intT* offsets) {
  parallel_for (intT i=0; i < nSegs; i++) {
    intT start = segments[i].start;
    intpair *Ci = C + start;
    intT l = segments[i].length;
    parallel_for_256 (intT j=0; j < l; j++) {
      intT o = Ci[j].second+offset;
      Ci[j].first = (o >= n) ? n-o : ranks[o];
    }
    if (l >= 256) 
      intSort::iSort(Ci, l, n ,utils::firstF<intT,intT>());
    else
      quickSort(Ci,l,pairCompF());
  }

  nextTimeM("sort");

  parallel_for (intT i=0; i < nSegs; i++) {
    intT start = segments[i].start;
    splitSegment(segOuts + offsets[i], start, segments[i].length, 
		 ranks, C + start, 1);
  }
  nextTimeM("split");
}

intT* suffixArray(uchar* ss, intT n) {
  // following line is used to fool icpc into starting the scheduler
  if (n < 0) cilk_spawn printf("ouch");
  //for (int i=0; i < n; i++) cout << "str[" << i << "] = " << s[i] << endl;
  intpair *C = newA(intpair,n);
  intT *ranks = newA(intT,n);
  uchar *s = newA(uchar,n);

  intT flags[256];
  for (intT i=0; i < 256; i++) flags[i] = 0;
  parallel_for (intT i=0; i < n; i++) 
    if (!flags[ss[i]]) flags[ss[i]] = 1;

  // renumber characters densely
  // start at 1 so that end-of-string is 0
  intT m = sequence::scan(flags,flags,256,utils::addF<intT>(),(intT)1);
  parallel_for (intT i=0; i < n; i++) 
    s[i] = flags[ss[i]];
  #ifdef printInfo
  cout << "m = " << m << endl;
  #endif

  intT bits = max(1,utils::log2Up(m));
  intT nchars = 32/bits;
  intT *foobar = ranks;

  // pack characters into word in chunks of "bits"
  startTime();
  parallel_for (intT i=0; i < n-nchars+1; i++) {
    C[i].first = grabChars(s+i,bits,nchars); 
    //foobar[i] = C[i].first;
    C[i].second = i;
  }

  for (intT i=max<intT>(n-nchars+1,0); i < n; i++) {
    C[i].first = grabCharsEnd(s+i,bits,nchars,n-i); 
    //foobar[i] = C[i].first;
    C[i].second = i;
  }
  free(s);

  nextTimeM("copy");
  intSort::iSort(C,n,(intT)1 << bits*nchars,utils::firstF<intT,intT>());
  nextTimeM("sort");

  seg *segOuts = newA(seg,n);
  seg *segments= newA(seg,n);
  intT *offsets = newA(intT,n);
  splitSegment(segOuts, 0, n, ranks, C, 1);
  nextTimeM("split");

  intT offset = nchars;
  
  int round =0;
  intT nKeys = n;
  while (1) {
    utils::myAssert(round++ < 40, "Suffix Array:  Too many rounds");
    intT nSegs = sequence::filter(segOuts,segments,nKeys,isSeg());
    if (nSegs == 0) break;

    parallel_for (intT i=0; i < nSegs; i++)
      offsets[i] = segments[i].length;

    nKeys = sequence::scan(offsets,offsets,nSegs,utils::addF<intT>(),(intT)0);
    #ifdef printInfo
    cout << "nSegs = " << nSegs << " nKeys = " << nKeys 
	 << " common length = " << offset << endl;
    #endif
    nextTimeM("filter and scan");    

    // parallel_for breaks the loop
    brokenCilk(nSegs, segments, C, offset, n, ranks, segOuts, offsets);

    offset = 2 * offset;
  }
  for (intT i=0; i < n; i++) ranks[i] = C[i].second;
  free(C); free(segOuts); free(segments); free(offsets); 
  //for (int i=0; i < n; i++) cout << "SA[" << i << "] = " << ranks[i] << endl;
  return ranks;
}
