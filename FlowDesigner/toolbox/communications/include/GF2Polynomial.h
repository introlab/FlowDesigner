// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#ifndef _GF2POLYNOMIAL_H_
#define _GF2POLYNOMIAL_H_

#include "Object.h"
#include "ParameterSet.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"

using namespace std;

namespace FD {

class GF2Polynomial{
	Vector<int> c;		// Vector of binary coefficients

public:
	GF2Polynomial();
	GF2Polynomial(Vector<int> const &p){ *this = p;};
	GF2Polynomial(String const &s);
	virtual ~GF2Polynomial(){};

	int degree() const {return c.size() - 1;}

	GF2Polynomial &upshift(int const l);

	GF2Polynomial &operator+=(GF2Polynomial const &right);
	GF2Polynomial &operator-=(GF2Polynomial const &right);
	GF2Polynomial &operator*=(GF2Polynomial const &right);
	GF2Polynomial &operator/=(GF2Polynomial const &right);
	GF2Polynomial &operator%=(GF2Polynomial const &right);

	GF2Polynomial &operator<<(String const &s);
	int operator[](int const i) const { return c[i];};

	GF2Polynomial const &operator=(Vector<int> const &c);

	size_t size() const { return c.size();};
};

GF2Polynomial operator+(GF2Polynomial const &left, GF2Polynomial const &right);

GF2Polynomial operator-(GF2Polynomial const &left, GF2Polynomial const &right);

GF2Polynomial operator*(GF2Polynomial const &left, GF2Polynomial const &right);

GF2Polynomial operator/(GF2Polynomial const &left, GF2Polynomial const &right);

GF2Polynomial operator%(GF2Polynomial const &left, GF2Polynomial const &right);


}	//namespace FD

#endif	// _GF2POLYNOMIAL_H_
