#ifndef MISC_H
#define MISC_H

/**Max function*/
template <class T>
T &max(T &a, T &b) {return a > b ? a : b;}

/**Min function*/
template <class T>
T &min(T &a, T &b) {return a < b ? a : b;}

/**Square function*/
template <class T>
inline T sqr(T x) {return x*x;}

/**Absolute value function*/
template <class T>
inline T abs(T x) {return x >= 0 ? x : -x;}


#endif
