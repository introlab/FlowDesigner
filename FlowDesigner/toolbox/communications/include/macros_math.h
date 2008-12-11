// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti



// Some useful constants
#define PI 3.141592653589793115997963468544


// Some useful macors

#define	MAX(a, b)	((a > b) ? (a) : (b))
#define MIN(a, b)	((a > b) ? (b) : (a))

#define ABS(a)		((a > 0) ? (a) : -(a))

#define BIN2GRAY(a)	(a ^ (a >> 1))

#define MAXSTAR(a, b)	(MAX(a, b) + log(1 + exp(-ABS(a - b))))
