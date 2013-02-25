// This code is part of the Problem Based Benchmark Suite (PBBS)
// Copyright (c) 2012 Guy Blelloch and the PBBS team
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
#include <cstring>
#include "parallel.h"
#include "IO.h"
#include "graph.h"
#include "graphIO.h"
#include "parseCommandLine.h"
#include "sequence.h"
#include "sequenceIO.h"
using namespace std;
using namespace benchIO;

// The serial spanning tree used for checking against
//pair<int*,int> st(edgeArray<int> EA);

// This needs to be augmented with a proper check

int parallel_main(int argc, char* argv[]) {
  commandLine P(argc,argv,"<inFile> <outfile>");
  pair<char*,char*> fnames = P.IOFileNames();
  char* iFile = fnames.first;
  char* oFile = fnames.second;

  graph<intT> G = readGraphFromFile<intT>(iFile);

  seqData Out = readSequenceFromFile(oFile);
  double *o = (double*) Out.A;
  intT n = Out.n;

  if (Out.dt != doubleT) {
    cout << "Wrong file type for the output --- expecting a sequence of doubles" << endl;
    return(1);
  }

  if (n != G.n) {
    cout << "Number of rows in matrix (" << G.n 
	 << ") does not equal number of elements in vector (" << n << ")." 
	 << endl;
    return(1);
  }

  for (int i=0; i < n; i++)
    if (G.V[i].degree != o[i]) {
      cout << "wrong value at location " << i << endl;
      return(1);
    }
  
  return 0;
}
