// Copyright (C) 1999 Jean-Marc Valin

#ifndef MISC_H
#define MISC_H

#include <vector>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/**Max function*/
template <class T>
T &max(T &a, T &b) {return a > b ? a : b;}

inline int max(int a, int b) {return a > b ? a : b;}
inline float max(float a, float b) {return a > b ? a : b;}


/**Min function*/
template <class T>
T &min(T &a, T &b) {return a < b ? a : b;}

inline int min(int a, int b) {return a < b ? a : b;}
inline float min(float a, float b) {return a < b ? a : b;}

/**Square function*/
template <class T>
inline T sqr(T x) {return x*x;}

/**Absolute value function*/
template <class T>
inline T abs(T x) {return x >= 0 ? x : -x;}


#ifdef __GNUC__
#define VAR_ARRAY
#endif


#if defined (VAR_ARRAY)  /* Prefered method is variable-size arrays is supported */

#define DYN_VEC(type, num, var) type var[num];

#elif defined (HAVE_ALLOCA_H)  /* Second best: alloca */

#include <alloca.h>

#define DYN_VEC(type, num, var) type *var=(type*)alloca((num)*sizeof(type));

#elif defined WIN32  /* On Win32, it's _alloca */

#include <malloc.h>
#define DYN_VEC(type, num, var) type *var=(type*)_alloca((num)*sizeof(type));

#else  /* When all else fails, allocate on the heap (but it's going to be slow) */

//#define DYN_VEC(type, num, var) vector<type> var(num);
template <class T>
class DynVec_ {
	T *array;
public:
	explicit DynVec_(int n) : array(new T[n]) {}
	~DynVec_() {delete [] array;}
	//T &operator[] (int i) {return array[i];}
	operator T* () {return array;}
};
#define DYN_VEC(type, num, var) DynVec_<type> var(num);

#endif



template<int M2>
inline int _log2(int i)
{
   if (i>>M2)
   {
      return M2+_log2<M2/2>(i>>M2);
   } else {
      return _log2<M2/2>(i);
   }
}

template<>
inline int _log2<1>(int i)
{
   if (i&2)
      return 1;
   else
      return 0;
}

inline int log2(int i)
{
   return _log2<16>(i);
}



#endif
