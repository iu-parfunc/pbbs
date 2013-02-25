#include <iostream>
#include <algorithm>
#include "parallel.h"
#include "sequence.h"
#include "sequenceIO.h"
#include "parseCommandLine.h"
#include "gettime.h"
#include <math.h>

using namespace std;
using namespace benchIO;
#define TTYPE double

template <class F>
void singleLocation(int n, bool randVals, F f){
    int b = INT_MAX;
    int* vals = newA(int,n);
    cilk_for(int i=0;i<n;i++){ vals[i]= (randVals ? utils::hash(i) : i); }
    startTime();
    //cilk_for(int i=0;i<n;i++) utils::writeMin(&b,vals[i]);
    cilk_for(int i=0;i<n;i++) f(&b,vals[i]);;
   
    if(randVals) cout << " (rand val to 1 location) :";
    else cout<<"(index i to 1 location) : ";
    nextTime();
    free(vals);
}

template <class F>
void contend(int numWriters, int numLocs, F f,  bool randDest, int numRounds,
	     bool chunked=0){
    int* A = newA(int,numLocs);
    int* vals = newA(int,numWriters);
    int* destinations = newA(int,numWriters);
    {cilk_for(int i=0;i<numWriters;i++)vals[i]=utils::hash(i);}
    {cilk_for(int i=0;i<numWriters;i++)destinations[i] = 
	(randDest ?  utils::hash(utils::hash(i)) : i) % numLocs;}
    {cilk_for(int i=0;i<numLocs;i++)A[i]=INT_MAX;}
    if(chunked) {
      startTime();
      int p = numWriters/numLocs;
      for(int r=0;r<numRounds;r++){
	cilk_for(int i=0;i<p;i++)
	  for(int j=0;j<numLocs;j++)
	    f(&A[j],vals[i]);
      }
      cout << "("<<p<<" chunks, each chunk writes rand val to location i%"<<numLocs<<" of "<<numLocs<<" locations in same order) :";
      nextTime();
    }
    else {
      startTime();
      for(int r=0;r<numRounds;r++)
	cilk_for(int i=0;i<numWriters;i++) f(&A[destinations[i]],vals[i]);
      if(randDest) cout << "(rand val to "<<numLocs<<" random locations) :";
      else cout << "(rand val to location i%"<<numLocs<<" of "<<numLocs<<" locations) :";
      nextTime();
    }
    free(A); free(vals); free(destinations);

}


template <class F>
void contendNew(int numWriters, int numLocs, F f,  bool randDest, int numRounds,
	     bool chunked=0){
    int* A = newA(int,numWriters);
    int* map = newA(int,numLocs);
    {cilk_for(int i=0;i<numLocs;i++) map[i] = utils::hash2(i) % numWriters; }
    int* vals = newA(int,numWriters);
    int* destinations = newA(int,numWriters);
    {cilk_for(int i=0;i<numWriters;i++)vals[i]=utils::hash(i);}
    {cilk_for(int i=0;i<numWriters;i++)destinations[i] = 
	map[(randDest ?  utils::hash(utils::hash(i)) : i) % numLocs];}
    {cilk_for(int i=0;i<numLocs;i++)A[i]=INT_MAX;}
    if(chunked) {
      startTime();
      int p = numWriters/numLocs;
      for(int r=0;r<numRounds;r++){
	cilk_for(int i=0;i<p;i++)
	  for(int j=0;j<numLocs;j++)
	    f(&A[j],vals[i]);
      }
      cout << "("<<p<<" chunks, each chunk writes rand val to location i%"<<numLocs<<" of "<<numLocs<<" locations in same order) :";
      nextTime();
    }
    else {
      startTime();
      for(int r=0;r<numRounds;r++)
	cilk_for(int i=0;i<numWriters;i++) f(&A[destinations[i]],vals[i]);
      if(randDest) cout << "(rand val to "<<numLocs<<" random locations) :";
      else cout << "(rand val to location i%"<<numLocs<<" of "<<numLocs<<" locations) :";
      nextTime();
    }
    free(A); free(vals); free(destinations); free(map);

}


template <class F>
void contendCount(int numWriters, int numLocs, F f,  bool randDest,
	     bool chunked=0){
    int* A = newA(int,numLocs);
    int* vals = newA(int,numWriters);
    int* destinations = newA(int,numWriters);
    int* Counts = newA(int,numWriters);
    {cilk_for(int i=0;i<numWriters;i++)vals[i]=utils::hash(i);}
    {cilk_for(int i=0;i<numWriters;i++)destinations[i] = 
	(randDest ?  utils::hash(utils::hash(i)) : i) % numLocs;}
    {cilk_for(int i=0;i<numLocs;i++)A[i]=INT_MAX;}
    if(chunked) {

      int p = numWriters/numLocs;
      cilk_for(int i=0;i<p;i++)
	for(int j=0;j<numLocs;j++)
	  Counts[i] = f(&A[j],vals[i]);
    
      cout << "("<<p<<" chunks, each chunk writes rand val to location i%"<<numLocs<<" of "<<numLocs<<" locations in same order) :";
      
    }
    else {
      cilk_for(int i=0;i<numWriters;i++) Counts[i] = f(&A[destinations[i]],vals[i]);
      if(randDest) cout << "(rand val to "<<numLocs<<" random locations) :";
      else cout << "(rand val to location i%"<<numLocs<<" of "<<numLocs<<" locations) :";

    }
    int* temp = newA(int,numWriters);
    {cilk_for(int i=0;i<numWriters;i++) temp[i] = Counts[i];}
    //for(int i=0;i<numWriters;i++)cout<<Counts[i]<<" ";cout<<endl;
    int maxAttempts = sequence::reduce(Counts,numWriters,utils::maxF<int>());
    int totalAttempts = sequence::plusScan(temp,temp,numWriters);
    free(A); free(vals); free(destinations); free(Counts); free(temp);
    cout << "max attempts = " << maxAttempts;
    cout << " total attempts = " << totalAttempts << endl;
}


struct writeMin { inline bool operator() (int* a, int b)  {return utils::writeMin(a,b);}};


inline bool fetchAdd2(int *a, int b) {
  int c; bool r=0;
  do c = *a; 
  while (!(r=utils::CAS(a,c,b)));
  return r;
}


struct fetchAdd { inline bool operator() (int* a, int b)  {return fetchAdd2(a,b);}};

inline bool CAS(int *a, int b) {
  int c; bool r=0;
  c = *a; 
  r=utils::CAS(a,c,b);
  return r;
}

struct CASOnce { inline bool operator() (int* a, int b)  {return CAS(a,b);}};

struct writeOnce {inline bool operator() (int* a, int b) {*a=b; return 1;}};

//struct readOnce { inline bool operator() (int* a, int b)  {return 1;}};


inline int writeMinCounter(int *a, int b) {
  int c; int r=0;
  do { c = *a; r++; } 
  while (c > b && !utils::CAS(a,c,b));
  return r;
}

struct writeMinCount { inline int operator() (int* a, int b) 
  {return writeMinCounter(a,b);}};


inline int fetchAddCounter(int *a, int b) {
  int c; int r=0;
  do { c = *a; r++; } 
  while (!utils::CAS(a,c,b));
  return r;
}

struct fetchAddCount 
{inline int operator() (int* a, int b) { return fetchAddCounter(a,b); }};


template <class F>
void doContend(int n, F f, int rounds)
{
  contend(n,n,f,0,rounds);

  for(int nl=1;nl<=n;nl*=2)
    contend(n,nl,f,1,rounds);
  contend(n,n,f,1,rounds);

  contend(n,n/40,f,0,rounds);
  contend(n,n/40,f,1,rounds);
  contend(n,n/40,f,1,rounds,1);

}

template <class F>
void doContendCount(int n, F f){
  contendCount(n,1,f,1);
  contendCount(n,n/40,f,1);
  contendCount(n,n,f,1);
  contendCount(n,n/40,f,1,1);
}  


template <class F>
void doContendNew(int n, F f, int rounds)
{
  contendNew(n,n,f,0,rounds);

  for(int nl=1;nl<=n;nl*=2)
    contendNew(n,nl,f,1,rounds);
  contendNew(n,n,f,1,rounds);

  contendNew(n,n/40,f,0,rounds);
  contendNew(n,n/40,f,1,rounds);
  contendNew(n,n/40,f,1,rounds,1);

}



int parallel_main (int argc, char* argv[]){
  int n = 1000;
  if (argc > 1) n = std::atoi(argv[1]);  
  // int numLocs = 50;
  // if (argc > 2) numLocs = std::atoi(argv[2]);
  int rounds = 1;
  if (argc > 2) rounds = std::atoi(argv[2]);
  cout<<"#writers = "<<n<<endl;
  cout<<"#rounds = "<<rounds<<endl;
 
  cout<<"-----------------WRITE WITH MIN---------------"<<endl;
  // singleLocation(n,0,writeMin());
  // singleLocation(n,1,writeMin());
  doContend(n,writeMin(),rounds);
  doContendCount(n,writeMinCount());
  doContendNew(n,writeMin(),rounds);

  cout<<"-----------------FETCH & ADD---------------"<<endl;

  doContend(n,fetchAdd(),rounds);
  doContendCount(n,fetchAddCount());
  doContendNew(n,fetchAdd(),rounds);

  cout<<"-----------------CAS---------------"<<endl;

  doContend(n,CASOnce(),rounds);
  doContendNew(n,CASOnce(),rounds);

  cout<<"----------------WRITE--------------"<<endl;
  doContend(n,writeOnce(),rounds);
  doContendNew(n,writeOnce(),rounds);

  // cout<<"-----------------READ ONCE---------------"<<endl;

  // doContend(n,readOnce(),rounds);
    
  // int p[4] = {40,80,160,320};
  // for(int i=0;i<4;i++){
  //   contend(n,n/p[i],0);
  //   contend(n,n/p[i],1);
  //   contend(n,n/p[i],1,1);
  // }
  

}

  // {
  //   int* A = new int[n];
  //   int* B = new int[n];
  //   int* I = new int[n];
  //   cilk_for (int i =0; i < n; i++) A[i] = B[i] = 0;
  //   cilk_for (int i=0; i < n; i++) I[i] = utils::hash(i)%n;
  //   timeStatement(cilk_for (int i=0;i<n;i++) B[I[i]] = A[i],"scatter (int)");
  //   free(A); free(B); free(I);
  // }

  // {
  //   TTYPE* A = new TTYPE[n];
  //   TTYPE* B = new TTYPE[n];
  //   int* I = new int[n];
  //   cilk_for (int i =0; i < n; i++) A[i] = B[i] = 0;
  //   cilk_for (int i=0; i < n; i++) I[i] = utils::hash(i)%n;
  //   timeStatement(cilk_for (int i=0;i<n;i++) B[I[i]] = A[i],"scatter (double)");
  //   cilk_for (int i=0;i<n;i++)  I[i] = I[i]%1000000;
  //   timeStatement(cilk_for (int i=0;i<n;i++) 
  // 		  B[I[i]] = A[i],"scatter contended (10^6)");
  //   cilk_for (int i=0;i<n;i++)  I[i] = I[i]%100000;
  //   timeStatement(cilk_for (int i=0;i<n;i++) 
  // 		  B[I[i]] = A[i],"scatter contended (10^5)");    
  //   free(A); free(B); free(I);
  // }

  // {
  //   int* A = new int[n];
  //   int* B = new int[n];
  //   int* I = new int[n];
  //   cilk_for (int i =0; i < n; i++) A[i] = B[i] = 0;
  //   cilk_for (int i=0; i < n; i++) I[i] = utils::hash(i)%n;
  //   timeStatement(cilk_for (int i=0;i<n;i++) 
  // 		  utils::CAS(&B[I[i]],0,A[i]),"scatter CAS (int)");
  //   cilk_for (int i=0;i<n;i++)  I[i] = I[i]%1000000;
  //   timeStatement(cilk_for (int i=0;i<n;i++) 
  // 		  utils::CAS(&B[I[i]],0,A[i]),"scatter CAS contended (10^6)");
  //   timeStatement(cilk_for (int i=0;i<n;i++) 
  // 		    if (B[I[i]] == 0) 
  // 		      utils::CAS(&B[I[i]],0,A[i]);,
  // 		  "scatter CAS contended conditional (10^6)");
  //   cilk_for (int i=0;i<n;i++)  I[i] = I[i]%100000;
  //   timeStatement(cilk_for (int i=0;i<n;i++) 
  // 		    utils::CAS(&B[I[i]],0,A[i]),
  // 		  "scatter CAS contended (10^5)");
  //   timeStatement(cilk_for (int i=0;i<n;i++) 
  // 		    if (B[I[i]] == 0) 
  // 		      utils::CAS(&B[I[i]],0,A[i]);,
  // 		  "scatter CAS contended conditional (10^5)");
  //   free(A); free(B); free(I);
  // }
