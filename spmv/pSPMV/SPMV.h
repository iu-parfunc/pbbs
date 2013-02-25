#include "parallel.h"
#include "graph.h"
using namespace std;

template <class VT>
struct mult {  VT operator() (VT a, VT b) {return a*b;} };

template <class VT>
struct add {  VT operator() (VT a, VT b) {return a+b;} };

template <>
struct mult<bool> {bool operator() (bool a, bool b) {return a&b;} };

template <>
struct add<bool> {bool operator() (bool a, bool b) {return a|b;} };

// multiply a compresses sparse row matrix
template <class intT, class E>
void MvMult(sparseRowMajor<E,intT> A, E* v, E* o, mult<E> fm, add<E> fa) {
  intT n = A.numRows;
  intT* starts = A.Starts; intT *cols = A.ColIds; E* vals = A.Values;
  parallel_for (intT i=0; i < n; i++) {
    intT s = starts[i];
    intT e = starts[i+1];
    E sum = 0;
    intT* cp = cols+s;
    intT* ce = cols+e;
    E* vp = vals+s; 
    while (cp < ce-1) {
      sum = fa(sum,fm(v[*cp],*vp));
      cp++; vp++;
      sum = fa(sum,fm(v[*cp],*vp));
      cp++; vp++;
    }
    if (cp < ce) 
      sum += v[*cp]*(*vp);
    o[i] = sum;
  }
}
