// Copyright (C) 1999 Jean-Marc Valin

#ifndef MISC_H
#define MISC_H

#include <vector>

#ifdef NO_MIN_MAX
/**Max function*/
template <class T>
T &max(T &a, T &b) {return a > b ? a : b;}


/**Min function*/
template <class T>
T &min(T &a, T &b) {return a < b ? a : b;}

#endif

/**Square function*/
template <class T>
inline T sqr(T x) {return x*x;}

/**Absolute value function*/
template <class T>
inline T abs(T x) {return x >= 0 ? x : -x;}

#if defined (VAR_ARRAY)  /* Prefered method is variable-size arrays are supported */

#define DYN_VEC(type, num, var) type var[num];

#include <alloca.h>

#elif defined (HAVE_ALLOCA_H)  /* Second best: alloca */

#define DYN_VEC(type, num, var) type *var=alloca((num)*sizeof(type));

#else  /* When all else fails, use an STL vector */

#define DYN_VEC(type, num, var) vector<type> var(num);

#endif




#endif
