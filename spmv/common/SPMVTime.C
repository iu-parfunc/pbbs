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
#include "gettime.h"
#include "utils.h"
#include "graph.h"
#include "parallel.h"
#include "IO.h"
#include "graphIO.h"
#include "graphUtils.h"
#include "sequenceIO.h"
#include "parseCommandLine.h"
using namespace std;
using namespace benchIO;

using namespace std;
using namespace benchIO;

typedef float eType;

void timeSPMV(sparseRowMajor<eType,intT> M, int rounds, char* outFile) {
  eType *v = newA(eType,M.numCols);
  eType *o = newA(eType,M.numRows);
  parallel_for (intT i=0; i < M.numCols; i++) v[i] = 1.0;
  MvMult<intT,eType>(M,v,o,mult<eType>(),add<eType>());
  for (int i=0; i < rounds; i++) {
    startTime();
    MvMult<intT,eType>(M,v,o,mult<eType>(),add<eType>());
    nextTimeN();
  }
  cout << endl;
  if (outFile != NULL) writeSequenceToFile(o, M.numCols, outFile);
  free(v);
  free(o);
}

int parallel_main(int argc, char* argv[]) {
  commandLine P(argc, argv, "[-o <outFile>] <matrixfile>");
  char* iFile = P.getArgument(0);
  char* oFile = P.getOptionValue("-o");
  int rounds = P.getOptionIntValue("-r",1);
  graph<intT> G = readGraphFromFile<intT>(iFile);
  sparseRowMajor<eType,intT> M = sparseFromGraph<eType>(G);
  M.Values = newA(eType,M.nonZeros);
  parallel_for (intT j=0; j < M.nonZeros; j++) M.Values[j] = 1.0;
  cout << "Columns = " << M.numCols << " Rows = " << M.numRows << " Non Zeros = " << M.nonZeros << endl;
  timeSPMV(M, rounds, oFile);
  M.del();
}
