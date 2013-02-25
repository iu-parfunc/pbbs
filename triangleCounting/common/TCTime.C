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
#include "plib/gettime.h"
#include "plib/utils.h"
#include "plib/graph.h"
#include "plib/parallel.h"
#include "plib/IO.h"
#include "plib/graphIO.h"
#include "plib/parseCommandLine.h"
#include "TC.h"
using namespace std;
using namespace benchIO;

void writeAnswer(intT ans, char *oFile)
{
  ofstream outF(oFile);
  
  if (outF.is_open()) {
    outF << ans << endl;
  }
  else {
    cerr << "ERROR: Cannot open output file" << endl;
    exit(0xff);
  }
}

void timeTC(graph<intT> G, int rounds, char* outFile) {
  graph<intT> GN = G.copy();
  intT ans;
  ans = countTriangle(GN);
  for (int i=0; i < rounds; i++) {
    GN.del();
    GN = G.copy();
    startTime();
    ans = countTriangle(GN);
    nextTimeN();
  }
  cout << endl;
  G.del();
  if (outFile != NULL) writeAnswer(ans, outFile);
  GN.del();
}

int parallel_main(int argc, char* argv[]) {
  commandLine P(argc,argv,"[-o <outFile>] [-r <rounds>] <inFile>");
  char* iFile = P.getArgument(0);
  char* oFile = P.getOptionValue("-o");
  int rounds = P.getOptionIntValue("-r",1);
  graph<intT> G = readGraphFromFile<intT>(iFile);
  timeTC(G, rounds, oFile);
}
