// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef GEN_TYPE_VECTOR_H
#define GEN_TYPE_VECTOR_H

#include "Object.h"
#include <vector>
#include "ObjectParser.h"
#include <ObjectRef.h>

using namespace std;

template<class T>
class Vector : public vector<T>, public Object
{
public:
   Vector() : vector<T>() {}
   explicit Vector(int n, const T &x = T()) : vector<T>(n, x) {}
   void printOn(ostream &out) const
   {
      out << *static_cast<const vector<T> *> (this);
   }
   virtual void rawWrite(ostream &out) const
   {
      out.write ((char*) (&(operator[](0))), int(size()*sizeof(T)));
   }
   
   void readFrom(istream &in=cin);

   virtual void destroy();

   static Vector<T> *alloc(int size);

};





template <class T>
inline void Vector<T>::readFrom(istream &in)
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

/*template <>
inline void Vector<FFLayer *>::readFrom(istream &in)
{
   int items_found=0;
   
   while (!in.eof())
   {
      FFLayer *tmp = new FFLayer;
      in >> *tmp;
      if (in.fail()) break;
      items_found++;
      resize(items_found);
      operator[] (items_found-1)=tmp;
   }
   in.clear();
   char ch;
   in >> ch;       
   }*/

//This thing's pissing me off!
class FFLayer;
template <>
inline void Vector<FFLayer*>::readFrom(istream &in)
{
   cerr << "fuck off!\n";
}

template <class T>
inline void Vector<T>::destroy()
{
   delete this;
}



#include "VectorPool.h"
extern VectorPool<float> floatVectorPool;
extern VectorPool<double> doubleVectorPool;

template <>
inline void Vector<float>::destroy()
{
   floatVectorPool.release(this);
}

template <>
inline void Vector<double>::destroy()
{
   doubleVectorPool.release(this);
}

template <class T>
inline Vector<T> *Vector<T>::alloc(int size)
{
   return new Vector<T> (size);
}


template <>
inline Vector<float> *Vector<float>::alloc(int size)
{
   return floatVectorPool.newVector(size);
}

template <>
inline Vector<double> *Vector<double>::alloc(int size)
{
   return doubleVectorPool.newVector(size);
}


/*template<class T>
istream &operator >> (istream &in, Vector<T> &vec)
{
   if (!isValidType(in, "Vector")) return in;
   vec.readFrom(in);
   return in;
   }*/


/**The object cast from ObjectRef*/
template <>
inline Vector<float> &object_cast<Vector<float> > (const ObjectRef &ref)
{
   Vector<float> *tmp = dynamic_cast<Vector<float> *>(&(*ref));
   if (!tmp)
   {
      Float *tmp2 = dynamic_cast<Float *>(&(*ref));
      if (!tmp2)
	 throw new CastException<Vector<float> > (typeid ((*ref)).name());
      Vector<float> *vec = Vector<float>::alloc(1);
      (*vec)[0] = (*tmp2).val();
   }
   return *tmp;
}

template <>
inline Vector<double> &object_cast<Vector<double> > (const ObjectRef &ref)
{
   Vector<double> *tmp = dynamic_cast<Vector<double> *>(&(*ref));
   if (!tmp)
   {
      Double *tmp2 = dynamic_cast<Double *>(&(*ref));
      if (!tmp2)
	 throw new CastException<Vector<double> > (typeid ((*ref)).name());
      Vector<double> *vec = Vector<double>::alloc(1);
      (*vec)[0] = (*tmp2).val();
   }
   return *tmp;
}

#endif
