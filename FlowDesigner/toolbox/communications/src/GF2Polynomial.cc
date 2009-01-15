// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "GF2Polynomial.h"

using namespace std;

namespace FD {

GF2Polynomial::GF2Polynomial()
{
	c.resize(0);
}


GF2Polynomial::GF2Polynomial(String const &s)
{
	unsigned int i, o;

	c.resize(0);
	for(i = 0; i < s.size(); i++)
	{
		o = int(s.data()[i] - '0');
		if(o < 0 || o >= 8) continue;		// Skip non-octal chars
		c.resize(c.size() + 1, (o >> 2) & 1);
		c.resize(c.size() + 1, (o >> 1) & 1);
		c.resize(c.size() + 1, o & 1);
	}
	while(c.size() && c.back() == 0) c.pop_back();
}

GF2Polynomial &GF2Polynomial::upshift(int const l)
{
	int i;
	GF2Polynomial &p = *this;

	c.resize(c.size() + l, 0);
	for(i = c.size() - 1; i >= l; i--)
		c[i] = c[i - l];

	for(; i >= 0; i--) c[i] = 0;

	return p;
}

GF2Polynomial &GF2Polynomial::operator<<(String const &s)
{
	unsigned int i, o;

	c.resize(0);
	for(i = 0; i < s.size(); i++)
	{
		o = int(s.data()[i] - '0');
		if(o < 0 || o >= 8) continue;		// Skip non-octal chars
		c.resize(c.size() + 1, (o >> 2) & 1);
		c.resize(c.size() + 1, (o >> 1) & 1);
		c.resize(c.size() + 1, o & 1);
	}
	while(c.size() && c.back() == 0) c.pop_back();

	return *this;
}

GF2Polynomial &GF2Polynomial::operator+=(GF2Polynomial const &p0)
{
	unsigned int i;

	if(c.size() < p0.size()) c.resize(p0.size(), 0);

	for(i = 0; i <= MIN(degree(), p0.degree()); i++)
	{
		c[i] ^= p0[i];
		c[i] &= 1;
	}

	return *this;
}

GF2Polynomial &GF2Polynomial::operator-=(GF2Polynomial const &p0)
{
	*this += p0;
	return *this;
}

GF2Polynomial &GF2Polynomial::operator*=(GF2Polynomial const &p0)
{
	int i, j;
	GF2Polynomial &p = *this;
	
	c.resize(p0.degree() + degree() + 1, 0);

	for(i = degree() - p0.degree(); i >= 0; i--)
	{
		if(c[i])
		{
			for(j = 1; j <= p0.degree(); j++)
			{
				c[i + j] ^= p0[j];
				c[i + j] &= 1;
			}
		}
	}
	
	return p;
}

GF2Polynomial &GF2Polynomial::operator/=(GF2Polynomial const &p0)
{
	int i, j;
	GF2Polynomial &p = *this;

	if(p0.degree() >= degree()) throw 1;

	for(i = degree(); i >= p0.degree(); i--)
	{
		if(c[i])
		{
			for(j = 0; j < p0.degree(); j++)
			{
				c[i - p0.degree() + j] ^= p0[j];
				c[i - p0.degree() + j] &= 1;
			}
		}
	}

	for(i = 0; i <= degree() - p0.degree(); i++)
		c[i] = c[i + p0.degree()];

	c.resize(c.size() - p0.size() + 1);

	return p;
}

GF2Polynomial &GF2Polynomial::operator%=(GF2Polynomial const &p0)
{
	GF2Polynomial &p = *this;

	p = p - ((p / p0) * p0);

	return p;
}


GF2Polynomial const &GF2Polynomial::operator=(Vector<int> const &v)
{
	int i;

	c.resize(0);
	for(i = 0; i < v.size(); i++) c.push_back(v[i]);

	return *this;
}

GF2Polynomial operator+(GF2Polynomial const &left, GF2Polynomial const &right)
{
	return GF2Polynomial(left) += right;
}

GF2Polynomial operator-(GF2Polynomial const &left, GF2Polynomial const &right)
{
	return GF2Polynomial(left) -= right;
}

GF2Polynomial operator*(GF2Polynomial const &left, GF2Polynomial const &right)
{
	return GF2Polynomial(left) *= right;
}

GF2Polynomial operator/(GF2Polynomial const &left, GF2Polynomial const &right)
{
	return GF2Polynomial(left) /= right;
}

GF2Polynomial operator%(GF2Polynomial const &left, GF2Polynomial const &right)
{
	return GF2Polynomial(left) %= right;
}



}	//namespace FD
