// Copyright (C) 1999 Jean-Marc Valin

#ifndef GEN_TYPE_ARRAY_H
#define GEN_TYPE_ARRAY_H

#include "Object.h"
#include "Vector.h"
#include "ObjectParser.h"
#include "misc.h"
#include "vec.h"

template<class T>
class Array : public Vector<T>
{
public:
   Array() : Vector<T>() {}
   explicit Array(int n, const T &x = T()) : Vector<T>(n, x) {}
   void printOn(ostream &out) const
   {
      out << *static_cast<const Vector<T> *> (this);
   }

   /*virtual void rawWrite(ostream &out) const
   {
      out.write ((const char*) &operator[](0), size()*sizeof(T));
      }*/
   
   void readFrom(istream &in=cin);
   
   virtual void destroy() {delete this;}

   Array<T> &operator+= (const Array<T> &v2) 
   {
      if (size() != v2.size())
	 cerr << "Array size mismatch\n";
      for (int i=0;i<size();i++)
	 operator[](i) += v2[i];
      return *this;
   }

   Array<T> &operator-= (const Array<T> &v2) 
   {
      if (size() != v2.size())
	 cerr << "Array size mismatch\n";
      for (int i=0;i<size();i++)
	 operator[](i) -= v2[i];
      return *this;
   }

   Array<T> operator+ (const Array<T> &v2) 
   {
      Array<T> v(*this);
      v += v2;
      return v;
   }

   Array<T> operator- (const Array<T> &v2) 
   {
      Array<T> v(*this);
      v -= v2;
      return v;
   }

   Array<T> operator- () 
   {
      Array<T> v(size());
      for (int i=0;i<size();i++)
	 v[i] = -operator[](i);
      return v;
   }
   
   T operator* (const Array<T> &v2) 
   {
      if (size() != v2.size())
	 cerr << "Array size mismatch\n";
      T sum=0;
      for (int i=0;i<size();i++)
	 sum += operator[](i)*v2[i];
      return sum;
   }

   Array<T> &operator*= (T scal) 
   {
      for (int i=0;i<size();i++)
	 operator[](i) *= scal;
      return *this;
   }

   Array<T> &operator/= (T scal) 
   {
      for (int i=0;i<size();i++)
	 operator[](i) /= scal;
      return *this;
   }

   Array<T> operator* (T scal) 
   {
      Array<T> v(*this);
      v *= scal;
      return v;
   }


   Array<T> operator/ (T scal) 
   {
      Array<T> v(*this);
      v /= scal;
      return v;
   }

   T norm() 
   {
      return sqrt(vec_norm2(&operator[](0), size()));
   }

   T norm2() 
   {
      return vec_norm2(&operator[](0), size());
   }

};



template <class T>
inline void Array<T>::readFrom(istream &in)
{
   int items_found=0;
   
   while (!in.eof())
   {
      T tmp;
      in >> tmp;
	 if (in.fail()) break;
	 items_found++;
	 resize(items_found);
	 operator[] (items_found-1)=tmp;
   }
   in.clear();
   char ch;
   in >> ch;       
}




#endif
