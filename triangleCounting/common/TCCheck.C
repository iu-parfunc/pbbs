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
#include "parallel.h"
#include "IO.h"
#include "graph.h"
#include "graphIO.h"
#include "parseCommandLine.h"
using namespace std;
using namespace benchIO;


intT readAnswer(char *oFile)
{
  ifstream ansF(oFile);

  if (ansF.is_open()) {
    intT ans;
    ansF >> ans;
    ansF.close();
    return ans;
  }
  else {
    cout << "TCCheck: Error opening user output file" << endl;
    exit(1);
  }
}
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
  // build the neighbors sets
  // cerr << "starting to build aux d/s" << endl;
  nbrs = new intSetT[G.n];
  parallel_for (intT i=0;i<G.n;i++) {

    // cerr << "inserting " << i << endl;
    for (intT dst=0;dst<G.V[i].degree;dst++) {
      intT t=G.V[i].Neighbors[dst];
      if (t>i) nbrs[i].insert(t);
    }
  }
  //  cerr << "building aux d/s fine" << endl;
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

int checkAnswer(graph<intT> G, intT ans)
{
  intT count = naiveCount(G);

  if (count == ans) 
    return 0; // good: the numbers match
  else {
    cout << "TCCheck: Answers mismatched (ans: " << ans 
	 << ", expecting: " << count << ")"  << endl;
    return 1;
  }
}

int parallel_main(int argc, char* argv[]) {
  commandLine P(argc,argv,"<inFile> <outfile>");
  pair<char*,char*> fnames = P.IOFileNames();
  char* iFile = fnames.first;
  char* oFile = fnames.second;

  graph<intT> G = readGraphFromFile<intT>(iFile);
  intT T = readAnswer(oFile);

  return checkAnswer(G, T);
}

