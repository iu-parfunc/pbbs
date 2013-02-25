#include <iostream>
#include <cstdlib>
#include "gettime.h"
#include "graph.h"
#include "parallel.h"
#include "quickSort.h"
#include <math.h>
#include <vector>
#include <fstream>
#include "sequence.h"
#include "utils.h"
#include "graphUtils.h"
using namespace std;

//Return memory size in bytes
inline long memorySize() {
  long mem;
  string token;
  ifstream file("/proc/meminfo",ios::in);
  while(file >> token) { 
    if(token == "MemTotal:") {
      if(!(file >> mem)) {
	cout<<"Error reading memory size\n";
	abort();       
      }
      break;
    }
  }
  file.close();
  return mem*1000;
}

inline void checkOverflow(long fgsize){
 if(fgsize > INT_T_MAX) {
    cout << "Error: size of memory greater than maximum value of INT_T\n";
    cout << "\tconsider recompiling with LONG=1\n";
    abort();
  }
}

bool gaussElim(intT* Starts, intT* ColIds, uintT n, uintT m, long fgsize){
  //preallocate space for fill graph
  intT* fillGraph = newA(intT,fgsize);
  uintT* fgOffsets = newA(uintT,n);
  uintT fgCurr = 0; //current index of fill graph array
  uintT* rchlink = newA(uintT,n);
  intT* mrglink = newA(intT,n);
  parallel_for(uintT i=0;i<n;i++) mrglink[i]=-1;

  //flags for mass symbolic elimination optimization
  //intT* massSymElim = newA(intT,n); 
  //for(intT i=0;i<n;i++) massSymElim[i]=-1;
  
  //for each column
  for(uintT i=0;i<n;i++){
    fgOffsets[i] = fgCurr;
    
    intT start = Starts[i];   
    intT end = Starts[i+1];

    rchlink[i] = n;
    // intT firstMrgLink = mrglink[i];
    // bool mseflag = 0;
    // bool skipMerge = 0;
    //if firstMrgLink is nonzero, set massSymElim[i] to
    //massSymElim[firstMrgLink], else set to i
    //massSymElim[i] = firstMrgLink ? massSymElim[firstMrgLink] : i;
    uintT numNonZeros = 0;
    intT rchm = i, t;
    //create link structure of nonzeros below diag
    for(intT j=start;j<end;j++){
      intT neighbor = ColIds[j];
      if(neighbor <= i) continue;
      do {
	t = rchm;
	rchm = rchlink[t];
      } while(rchm <= neighbor);
      rchlink[t] = neighbor;
      rchlink[neighbor] = rchm;
      rchm = t;
      numNonZeros++;
      // if(!mseflag && massSymElim[neighbor] != massSymElim[i])
      //mseflag = 1;
    }

    //if eligible for mass symbolic elimination
    //then skip merge phase
    //must satisfy conditions: mseflag is 0 (all neighbors
    //of i are in first mrglink), firstMrgLink is not null,
    //and mrglink[firstMrgLink] is null (exactly one 
    //representative)
    // if(!mseflag && firstMrgLink && !mrglink[firstMrgLink])
    //   skipMerge = 1;
    //link through columns < i which affect i
    intT k = i;
    while((k = mrglink[k]) != -1){ //cout << " i = " <<i<<" k = "<<k<<endl;
      //merge stuff
      intT colStart = fgOffsets[k];
      intT colEnd = fgOffsets[k+1];
      rchm = i;
      for(intT j=colStart;j<colEnd;j++){
	intT neighbor = fillGraph[j];
	if(neighbor <= i) continue;
	//check if neighbor exists
	do {
	  t = rchm;
	  rchm = rchlink[t];	  
	} while(rchm < neighbor);

	if(rchm != neighbor){
	  //if not duplicate, add it
	  rchlink[t] = neighbor;
	  rchlink[neighbor] = rchm;
	  rchm = neighbor;
	  numNonZeros++;
	}
      }
    }

    if(fgCurr+numNonZeros >= fgsize) {
      free(fillGraph); free(fgOffsets); free(rchlink); free(mrglink);
      return 0;
    }

    //copy stuff into fill graph
    k = i;
    for(uintT j=0;j<numNonZeros;j++){
      fillGraph[fgCurr++] = (k = rchlink[k]);
    }
    if(numNonZeros){ //if at least one neighbor
      //update mrglink array
      k = fillGraph[fgOffsets[i]];
      
      mrglink[i] = mrglink[k];
      mrglink[k] = i;
    }
  }
  cout<<"Number of nonzeros = " << fgCurr << endl;
  free(fillGraph); free(fgOffsets); free(rchlink); free(mrglink);
  return 1;
}


void ge(graph<intT> G) {
  intT n = G.n;
  vertex<intT>* vtx = G.V;

  startTime();
 
  // sort neighbors in adjacency list by id
  parallel_for(uintT i=0;i<n;i++){
    compSort(vtx[i].Neighbors,vtx[i].degree,less<intT>());
  }
  nextTime("Sort");

  sparseRowMajor<intT,intT> GS = sparseFromGraph<intT>(G);

  nextTime("Convert to sparse row major format");
  int m = GS.nonZeros;
  //determine space for fill graph
  long fgsize = min((long)n*(n-1)/2,min((long)32*(n+m),(long)memorySize()/10));

  checkOverflow(fgsize);
  bool success = gaussElim(GS.Starts,GS.ColIds,GS.numRows, GS.nonZeros,fgsize);
  nextTime("Gaussian elimination first try");
  if(!success){
    cout << "Not enough memory\nRetrying with more memory...\n";
    fgsize = min((long)n*(n-1)/2,memorySize()/10);
    checkOverflow(fgsize);
    success = gaussElim(GS.Starts,GS.ColIds,GS.numRows, GS.nonZeros,fgsize);
    nextTime("Gaussian elimination second try");
    if(!success) cout << "Gaussian elimination failed - not enough memory\n";
    else cout << "Second try succeeded\n";
  } else cout << "First try succeeded\n";
  GS.del();

}

