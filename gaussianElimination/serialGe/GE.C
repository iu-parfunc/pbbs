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

void gaussElim(intT n, vertex<intT>* G, intT m){
  //int totalDegree = 2*m;
  //float density = totalDegree/n;
  //float threshold = 10*sqrt(n);
  
  //keep track of which vertices are using new memory,
  //which is freeable
  bool* hasNew = newA(bool,n); 
  memset(hasNew,0,n);
  
  //testing
  intT fill=0;
  long numOps=0;

  //remove self and make clique of neighbors
  for(intT i=0;i<n;i++){
    intT v = i;
    /*
    if(density > threshold) {
      cout<<"Terminating early because graph became too dense"<<endl;
      cout<<"Number of vertices remaining: "<<i+1<<endl;
      cout<<"Number of edges remaining: "<<totalDegree/2<<endl;
      break;
      }*/
    intT b = G[v].degree;
    //int numNew = 0;
    //looping through neighbors
    for(intT j=0;j<b;j++){
      intT ngh = G[v].Neighbors[j];
      intT a = G[ngh].degree;
      intT maxNewDegree = min(a+b-2,n-i-1);
      intT* newNeighbors = newA(intT,maxNewDegree);
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
	} 
	else if(bi >= b) { //b is empty 
	  newNeighbors[k++] = G[ngh].Neighbors[ai++];
	} 
	else { //both not empty
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
      //update number of new edges and new degrees
      intT newEdges = k-G[ngh].degree;
      //numNew += newEdges;
      fill+=1+newEdges;
      G[ngh].degree = k;
      if(hasNew[ngh]){ //vertex currently using freeable memory
	free(G[ngh].Neighbors);
      }
      else hasNew[ngh] = 1;
      
      G[ngh].Neighbors = newNeighbors;
      numOps+=k;
    }
    //update number of new edges and density
    //totalDegree += numNew - G[v].degree;
    //if(i) density = totalDegree/i;
  }
  free(hasNew);
  cout<<"Fill: "<<fill<<endl;
  cout<<"(metis fill) Fill/2 + #original edges: "<<fill/2+m/2<<endl;
  cout<<"NumOps: "<<numOps<<endl;
}


void ge(graph<intT> G) {
  intT n = G.n;
  vertex<intT>* vtx = G.V;
  startTime();
 
  //sort neighbors in adjacency list by id
  for(intT i=0;i<n;i++){
    compSort(vtx[i].Neighbors,vtx[i].degree,less<intT>());
  }
  nextTime("Sort");
  gaussElim(n,vtx,G.m);
  nextTime("Gaussian elimination minus sort (2dmesh)");

}

