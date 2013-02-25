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
#include "unionFind.h"
#include "blockRadixSort.h"
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

void ge(intT* Starts, intT* ColIds, uintT n, intT* elimTree, intT* offsets, intT root, uintT* rchlinks){
  intT start = offsets[root];
  intT end = offsets[root+1];
  if(end == start){ }
  else if(end-start == 1){
    ge(Starts,ColIds,n,elimTree,offsets,elimTree[start],rchlinks);
  }
  else {
    for(intT i = start; i < end;i++){
      intT child = elimTree[i];
      cilk_spawn ge(Starts,ColIds,n,elimTree,offsets,child,rchlinks);
    } cilk_sync;
  }
  //eliminate column "root"
  uintT* rchlink = rchlinks+root*n;
    
  intT sstart = Starts[root];   
  intT send = Starts[root+1];
  intT rchm = root, t;

  rchlink[root] = n;
  //create link structure of nonzeros below diag
  for(intT j=sstart;j<send;j++){
    intT neighbor = ColIds[j];
    if(neighbor <= root) continue;
    do { 
      t = rchm;
      rchm = rchlink[t];
    } while(rchm <= neighbor);
    rchlink[t] = neighbor;
    rchlink[neighbor] = rchm;
    rchm = t;
  }


  //link through children columns of root 
  for(intT i = start; i < end; i++){
    intT child = elimTree[i];
    //merge stuff
    uintT* ch_rchlink = rchlinks+child*n;
    intT rchm = root, t;
    intT neighbor = child;
    while((neighbor = ch_rchlink[neighbor]) != n){
   
      if(neighbor > rchm) { 
	//loop through root's list
	do {
	  t = rchm;
	  rchm = rchlink[t];
	} while(neighbor > rchm);
	//not duplicate
	if(neighbor != rchm){
	  rchlink[t] = neighbor;
	  rchlink[neighbor] = rchm;
	}
	//back up one
	rchm = t;

      }
    } 
  }
}

bool gaussElim(intT* Starts, intT* ColIds, uintT n, uintT m,
	       intT* elimTree, intT* offsets, intT root){

  uintT* rchlinks = newA(uintT,n*n);
  if(!rchlinks) {
    cout << "malloc failed\n"; 
    abort();
  }

  intT* fill = newA(intT,n);

  ge(Starts,ColIds,n,elimTree,offsets,root,rchlinks);
  parallel_for(intT i=0;i<n;i++){
    intT nonzeros = 0;
    uintT* rchlink = rchlinks+n*i;
    intT rchm = rchlink[i];
    while(rchm != n){
      rchm = rchlink[rchm];
      nonzeros++;
    }
    fill[i] = nonzeros;
  }
  intT nonzeros = sequence::plusScan(fill,fill,n);
  
  cout<<"nonzeros = " << nonzeros << endl;
  free(fill); free(rchlinks); 

  return 1;
}

typedef pair<intT,intT> intTPair;

inline void initParents(intTPair* parents, intT n){
  parallel_for(intT i=0;i<n;i++) {
    parents[i].first=i;
    parents[i].second=n;
  }
}

intTPair* computeElimTree(intT* Starts, intT* ColIds, uintT n){
  //if second element = n, then there is no parent
  unionFind UF(n);
  intTPair* parents = newA(intTPair,n);
  initParents(parents,n);
  for(intT i=0;i<n;i++){
       
    intT start = Starts[i];   
    intT end = Starts[i+1]; 

    for(intT j=start;j<end;j++){
      intT neighbor = ColIds[j];
      if(neighbor > i) break;
      intT r = UF.find(neighbor);
      if(r != i){
	parents[r].second=i; //not path-compressed
	UF.link(r,i);
      }
    }
  }
  UF.del();
  return parents;
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
  intT m = GS.nonZeros;

  intT* elimTree = newA(intT,n);
  intT* offsets = newA(intT,n+2);

  intTPair* parents = computeElimTree(GS.Starts,GS.ColIds,GS.numRows);
  intT root;
  parallel_for(intT i=0;i<n;i++) if(parents[i].second == n) root = i;

  //sort pairs based on parent id
  intSort::iSort(parents,n,n+1,utils::secondF<intT,intT>());

  //copy children over
  parallel_for(intT i=0;i<n;i++) elimTree[i] = parents[i].first;

  //compute offsets of children array per parent
  offsets[n+1] = n;
  parallel_for(intT i=0;i<n+1;i++) offsets[i] = INT_T_MAX;

  offsets[parents[0].second+1] = 0;

  parallel_for(intT i=1;i<n;i++) {
    if(parents[i].second != parents[i-1].second)
      offsets[parents[i].second+1] = i;
  }
  free(parents);
  sequence::scanBack(offsets, offsets, n+2, utils::minF<intT>(),
		     (intT)INT_T_MAX);
  offsets[n+1] = n;

  nextTime("Compute elimination tree");

  bool success = gaussElim(GS.Starts,GS.ColIds,GS.numRows, GS.nonZeros, elimTree, offsets, root);
  nextTime("Gaussian elimination first try");
 
  free(offsets); free(elimTree);
  GS.del();

}

