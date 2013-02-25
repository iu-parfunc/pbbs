#include <iostream>
#include <cstdlib>
#include "gettime.h"
#include "graph.h"
#include "cilk.h"
#include "quickSort.h"
#include <math.h>
#include <vector>
#include <fstream>
#include "sequence.h"
#include "utils.h"
using namespace std;

timer mm;

//Return memory size in bytes
long memorySize() {
  long mem;
  string token;
  ifstream file("/proc/meminfo",ios::in);
  while(file >> token) {
    if(token == "MemTotal:") {
      if(!(file >> mem)) {
	cout<<"Error reading memory size\n";
	return 0;       
      }
      break;
    }
  }
  file.close();
  return mem*1000;
}

void gaussElim(intT* vtx, intT n, vertex<intT>* G, intT m){
  intT* reserve = newA(intT,n);
  parallel_for(intT i=0;i<n;i++) reserve[i]=-1;

  intT end = n;
  intT wasted = 0;
  intT ratio = 100;
  intT maxR = 1 + n/ratio;
  int round = 0;
  intT *hold = newA(intT,maxR); //temp array for pack
  bool *keep = newA(bool,maxR); //0 if win, 1 if lose
  
  ulong spaceUsed = 0;
  //int totalDegree = 2*m;
  //float density = totalDegree/end;
  //float threshold = 10*sqrt(n);
  long fill = 0;
  long ops = 0;

  vector<intT*> allocatedMemory;
  long memory = memorySize()/10; //specific to machine
  intT* numNghs = newA(intT,end);
  while(end > 0 /*&& density < threshold*/){
    //****MEMORY CLEANUP IF NECESSARY****
    mm.start();
    if(spaceUsed > memory) { 
      //copy all edges of alive vertices to new array
      //and free allocated memory
      //int* numNghs = newA(int,end);
      parallel_for(intT i=0;i<end;i++){
	intT v = vtx[i];
	numNghs[i] = G[v].degree;
      }
      intT memNeeded = sequence::plusScan(numNghs,numNghs,end);
      intT* newSpace = newA(intT,memNeeded);
      //copy to new memory
      parallel_for(intT i=0;i<end;i++){
	intT v = vtx[i];
	intT offset = numNghs[i];
	parallel_for(intT j=0;j<G[v].degree;j++){
	  newSpace[offset+j] = G[v].Neighbors[j];
	}
	G[v].Neighbors = newSpace + offset;
      }

      //free(numNghs);

      //free old memory
      parallel_for(intT i=0;i<allocatedMemory.size();i++){
	free(allocatedMemory[i]);
      }
      allocatedMemory.clear();
      //add new memory to allocatedMemory
      allocatedMemory.push_back(newSpace);
      spaceUsed = memNeeded;
    }
    mm.stop();
    round++;
    intT size = min(maxR,end); 
    intT start = end - size;

    //****RESERVE VERTICES****
    //reserve self and neighbors
    parallel_for(intT i=0;i<size;i++){ 
      intT v = vtx[start+i];
      hold[i] = v;
      utils::writeMax(&reserve[v],v); //write to self
      for(intT j=0;j<G[v].degree;j++){ //write to neighbors
	intT ngh = G[v].Neighbors[j];
	utils::writeMax(&reserve[ngh],v);
      }
    }

    //****FIND WINNERS****
    //check if won, and compute number of neighbors
    //int* numNghs = newA(int,size);
    parallel_for(intT i = 0;i<size;i++){
      intT v = vtx[start+i];
      keep[i] = (v != reserve[v]); //0 if win, 1 if lose
      if(!keep[i]){
	for(intT j=0;j<G[v].degree;j++){
	  intT ngh = G[v].Neighbors[j];
	  if((keep[i] = (v != reserve[ngh])))
	    break;
	}
      }
      if(!keep[i]){
	numNghs[i] = G[v].degree;
      } 
      else numNghs[i] = 0;
      
    }

    //****MEMORY ALLOCATION****
    //compute total number of neighbors of winners
    intT s = sequence::plusScan(numNghs,numNghs,size);
    //compute total number of neighbors of winners and 
    //the number of neighbors of the winners' neighbors
    intT* numNghsNghs = newA(intT,s);
    parallel_for(intT i=0;i<size;i++){
      if(!keep[i]){
	intT v = vtx[start+i];
	intT offset = numNghs[i];
	parallel_for(intT j=0;j<G[v].degree;j++){
	  intT ngh = G[v].Neighbors[j];
	  //set max new degree 
	  numNghsNghs[offset+j] = 
	    min(G[ngh].degree+G[v].degree-2,end-1);
	}
      }
    }
    intT t = sequence::plusScan(numNghsNghs,numNghsNghs,s);
    //allocate memory for new neighbors
    intT* space = newA(intT,t);
    spaceUsed += t;
    allocatedMemory.push_back(space);


    //****BUILD CLIQUES****
    intT* numFill = newA(intT, s);
    intT* numOps = newA(intT,s);
    //if won, remove self and make clique of neighbors
    parallel_for(intT i=0;i<size;i++){
      if(!keep[i]){ //check if won
	intT v = vtx[start+i];
	//maybe make this loop not parallel?
	parallel_for(intT j=0;j<G[v].degree;j++){
	  intT ngh = G[v].Neighbors[j];
	  intT a = G[ngh].degree;
	  intT b = G[v].degree;
	  intT offset = numNghsNghs[numNghs[i]+j];
	  intT* newNeighbors = space + offset;

	  //merge adjacency lists
	  //remove duplicates, self edges, and edge to winner
	  intT ai = 0; intT bi = 0; intT k = 0;
	  while(1){
	    if(ai >= a && bi >=b) break;
	    //skipping self edges and edges to winner	     
	    else if(ai < a && G[ngh].Neighbors[ai] == v){
	      ai++;
	    }
	    else if(bi < b && G[v].Neighbors[bi] == ngh){
	      bi++;
	    }
	    //merge
	    else if(ai >= a) { //a is empty
	      newNeighbors[k++] = G[v].Neighbors[bi++];
	    } else if(bi >= b) { //b is empty 
	      newNeighbors[k++] = G[ngh].Neighbors[ai++];
	    } else { //both not empty
	      intT aval = G[ngh].Neighbors[ai];
	      intT bval = G[v].Neighbors[bi];
	      if(aval < bval) newNeighbors[k++] = G[ngh].Neighbors[ai++];
	      else if(bval < aval) newNeighbors[k++] = G[v].Neighbors[bi++];
	      else { //equal case (removing duplicate edge)
		newNeighbors[k++] = G[ngh].Neighbors[ai++]; 
		bi++;
	      }
	    }
	  }
	  //update number of new edges and degrees
	  numFill[numNghs[i]+j] = 1+k-G[ngh].degree;
	  numOps[numNghs[i]+j] = k;
	  G[ngh].degree = k;
	  G[ngh].Neighbors = newNeighbors;
	}
	//numNewDegree[numNghs[i]] -= G[v].degree;
      }
    }
    fill += sequence::plusScan(numFill,numFill,s);
    ops += sequence::plusScan(numOps,numOps,s);


    //free(numNghs);
    free(numNghsNghs);
    free(numFill); free(numOps);
 
    //****PHASE CLEANUP****
    //pack out winners, reset reserves, update number of edges
    //and density
    intT nn = sequence::pack(hold,vtx+start,keep,size);
    end = start + nn;
    wasted += nn;
    parallel_for(intT i=0;i<end;i++) {
      if(reserve[vtx[i]] != -1)
	reserve[vtx[i]]=-1;
    }
    //totalDegree += numNew;
    //if(end) density = totalDegree/end;
  }
  free(keep);
  free(hold);
  free(numNghs);
  /*
  if(end){ 
    cout<<"Terminated early because graph became too dense"<<endl;
    cout<<"Vertices remaining = "<<end<<endl;
    cout<<"Edges remaining = "<<totalDegree/2<<endl;
    }*/
  cout << "Fill : " << fill << endl;
  cout<<"(metis fill) Fill/2 + #original edges: "<<fill/2+m/2<<endl;
  cout << "Num ops : " << ops << endl;
}


void ge(graph<intT> G) {
  intT n = G.n;
  cout<<"n = "<<n<<endl;
  //seq<vindex> vertices = seq<vindex>(n, utils::identityF<int>());
  intT* vertices = newA(intT,n);
  parallel_for(intT i=0;i<n;i++) vertices[i] = i;

  vertex<intT>* vtx = G.V;
  startTime();
  //sort neighbors in adjacency list by id
  parallel_for(intT i=0;i<n;i++){
    quickSort(vtx[i].Neighbors,vtx[i].degree,less<intT>());    
  }
  nextTime("Sort");
  gaussElim(vertices,n,vtx,G.m);
  nextTime("Gaussian elimination minus sort (2dmesh)");

  cout<<"Time spent freeing wasted memory and copying: "<<mm.total()<<endl;
  free(vertices);
}
