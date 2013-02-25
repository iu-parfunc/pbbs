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

#include "graph.h"
#include "utils.h"
#include "parallel.h"
using namespace std;

intT maxMatchSerial(edge<intT>* E, intT* vertices, intT* out, intT m) {
  intT k=0;
  for (intT i = 0; i < m; i++) {
    intT v = E[i].v;
    intT u = E[i].u;
    if(vertices[v] < 0 && vertices[u] < 0){
      vertices[v] = u;
      vertices[u] = v;
      out[k++] = i;
    }
  }
  return k;
}

// Finds a maximal matching of the graph
// Returns cross pointers between vertices, or -1 if unmatched
pair<intT*,intT> maximalMatching(edgeArray<intT> EA) {
  intT m = EA.nonZeros;
  intT n = EA.numRows;
  cout << "n=" << n << "m=" << m << endl;

  intT* vertices = newA(intT,n);
  for(intT i=0; i<n; i++) vertices[i] = -1;
  intT* out = newA(intT,m);
  intT size = maxMatchSerial(EA.E, vertices, out, m);
  free(vertices);

  return pair<intT*,intT>(out,size);
}  

