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
#include <fstream>
#include "gettime.h"
#include "utils.h"
#include "graph.h"
#include "parallel.h"
#include "IO.h"
#include "graphIO.h"
#include "graphUtils.h"
#include "parseCommandLine.h"
#include <math.h>
#include "GE.h"

using namespace std;
using namespace benchIO;

int parallel_main(int argc, char* argv[]) {

  commandLine P(argc, argv, "[-o <outFile>] [-r <rounds>] [-p <permutation file>] <inFile>");
  char* iFile = P.getArgument(0);
  char* oFile = P.getOptionValue("-o");
  int rounds = P.getOptionIntValue("-r",1);
  char* permFile = P.getOptionValue("-p");
  bool reverse = P.getOption("-reverse");
  bool noreorder = P.getOption("-noreorder");
  graph<intT> G = readGraphFromFile<intT>(iFile);

  intT n = G.n;
  intT* iPerm = new intT[n];
  if(noreorder){
    cout << "Using original ordering of vertices as given in file" << endl;
  }
  else if(permFile != NULL){ 
    ifstream file (permFile, ios::in);
    if (!file.is_open()) {
      std::cout << "Unable to open file: " << permFile << std::endl;
      abort();
    }
    for(intT i=0;i<n;i++) 
      if(!(file >> iPerm[i])) {
	cout << "Bad permutation file " << endl;
	abort();
      }

    cout << "Using permutation file" << endl;

  } else {
    bool* flags = newA(bool,n);
    parallel_for(intT i=0;i<n;i++)flags[i]=0;

    intT* ordering = newA(intT,n);
    intT rootn = sqrt(n);
    intT logrootn = ceil(log2(rootn));
    intT mm = 0;
    for(intT i=2;i<logrootn+1;i++){
      intT size = min<intT>(1 << i,rootn);
      for(intT j=0;j<=rootn-size;j=j+size){
	for(intT h=0;h<=rootn-size;h=h+size){
	  //add internal nodes to ordering
	  for(intT k=1;k<size-1;k++){
	    for(intT l=1;l<size-1;l++){
	      intT offset = (j+k)*rootn+(l+h);
	      if(!flags[offset]){
		ordering[mm++] = offset;
		flags[offset] = 1;
	      }
	    }
	  }
	}
      }
    }
    for(intT i=0;i<n;i++){
      if(!flags[i]) {
	ordering[mm++] = i;
      }
    }
    free(flags);
    parallel_for(intT i=0;i<n;i++) iPerm[ordering[i]] = i;
    free(ordering);
    cout << "Using nested dissection heuristic" << endl;
  }  
  
  //reverse order of graph (needed for parallel version)
  if(reverse)
    parallel_for(intT i=0;i<n;i++) iPerm[i]=n-1-iPerm[i];
  if(!noreorder)
    G = graphReorder(G,iPerm);
  free(iPerm);

  startTime();
  ge(G);
  stopTime(.1,"Gaussian elimination");
  G.del(); 


}
