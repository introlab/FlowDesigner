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


#if defined (VAR_ARRAY)  /* Prefered method is variable-size arrays are supported */

#define DYN_VEC(type, num, var) type var[num];

#elif defined (HAVE_ALLOCA_H)  /* Second best: alloca */

#include <alloca.h>

#define DYN_VEC(type, num, var) type *var=(type*)alloca((num)*sizeof(type));

#elif defined WIN32

#include <malloc.h>
#define DYN_VEC(type, num, var) type *var=(type*)_alloca((num)*sizeof(type));

#else  /* When all else fails, use an STL vector */

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




#endif
