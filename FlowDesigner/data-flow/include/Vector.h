// Copyright (C) 1999 Jean-Marc Valin

#ifndef GEN_TYPE_VECTOR_H
#define GEN_TYPE_VECTOR_H

#include "Object.h"
#include <vector>
#include "ObjectParser.h"
#include "ObjectRef.h"
#include "binio.h"
#include "typetraits.h"

using namespace std;

/**The (template) Overflow Vector type, it adds functionnality to the 
   basic vector<T> type, including serialisation and unserialisation
   @author Jean-Marc Valin
*/
template<class T>
class Vector : public vector<T>, public Object
{
public:
   Vector() : vector<T>() {}
   explicit Vector(int n, const T &x = T()) : vector<T>(n, x) {}
   void printOn(ostream &out) const
   {
      out << className();
      for (int i=0; i < size(); i++)
      {
	 out << " " << operator[](i);
      }
      out << " > ";
      //out << *static_cast<const vector<T> *> (this);
   }
   
   void readFrom(istream &in=cin);

   virtual void serialize(ostream &out) const;

   virtual void unserialize(istream &in);

   virtual void destroy();

   static Vector<T> *alloc(int size);

};


template <class T>
inline void _vector_readFrom(Vector<T> &v, istream &in)
{
   while (1)
   {
      char ch=' ';
      while (ch == ' ')
      {
	 in >> ch;
	 if (ch == '>')
	 {
	    return;
	 } else if (ch != ' ') {
	    in.putback(ch);
	 }
	 if (in.fail()) 
	    throw new GeneralException("Error reading Vector: '>' expected", __FILE__, __LINE__);
      }
      T tmp;
      in >> tmp;
      if (in.fail()) 
	 throw new GeneralException("Error reading Vector", __FILE__, __LINE__);
      v.push_back(tmp);
   }
}


template <class T>
inline void _vector_readFrom(Vector<T*> &v, istream &in)
{
   while (1)
   {
      char ch=' ';
      while (ch == ' ')
      {
	 in >> ch;
	 if (ch == '>')
	 {
	    return;
	 } else if (ch != ' ') {
	    in.putback(ch);
	 }
	 if (in.fail()) 
	    throw new GeneralException("Error reading Vector: '>' expected", __FILE__, __LINE__);
      }
      T *tmp = new T;
      in >> *tmp;
      if (in.fail()) 
	 throw new GeneralException("Error reading Vector", __FILE__, __LINE__);
      v.push_back(tmp);
   }
}


template <class T>
inline void Vector<T>::readFrom(istream &in)
{
   _vector_readFrom(*this, in);
}



//FIXME: Serialize problems with (Object *)
template<class T, int I>
struct VecBinary {
   static inline void serialize(const Vector<T> &v, ostream &out)
   {
      throw new GeneralException("VecBinary default serialize should never be called", __FILE__, __LINE__);
   }
   static inline void unserialize(Vector<T> &v, istream &in)
   {
      throw new GeneralException("VecBinary default unserialize should never be called", __FILE__, __LINE__);
   }
};

template<class T>
struct VecBinary<T,TTraits::Object> {
   static inline void serialize(const Vector<T> &v, ostream &out)
   {
      out << "{" << v.className() << endl;
      out << "|";
      int tmp=v.size();
      BinIO::write(out, &tmp, 1);
      for (int i=0;i<v.size();i++)
      {
	 out << "|";
	 v[i].serialize(out);
	 out << "|";
      }
      out << "}";
   }
   static inline void unserialize(Vector<T> &v, istream &in)
   {
   }
};

template<class T>
struct VecBinary<T,TTraits::Basic> {
   static inline void serialize(const Vector<T> &v, ostream &out)
   {
      out << "{" << v.className() << endl;
      out << "|";
      int tmp=v.size();
      BinIO::write(out, &tmp, 1);
      BinIO::write(out, &(const_cast<Vector<T> &>(v))[0], v.size());
      out << "}";
   }
   static inline void unserialize(Vector<T> &v, istream &in)
   {
   }
};

template<class T>
struct VecBinary<T,TTraits::Unknown> {
   static inline void serialize(const Vector<T> &v, ostream &out)
   {
      throw new GeneralException(string("Sorry, can't serialize this kind of object (") + typeid(T).name()
				 + ")", __FILE__, __LINE__);
   }
   static inline void unserialize(Vector<T> &v, istream &in)
   {
      throw new GeneralException(string("Sorry, can't unserialize this kind of object (") + typeid(T).name()
				 + ")", __FILE__, __LINE__);
   }
};


template <class T>
inline void Vector<T>::serialize(ostream &out) const
{
   VecBinary<T, TypeTraits<T>::kind>::serialize(*this, out);
}

template <class T>
inline void Vector<T>::unserialize(istream &in)
{
   VecBinary<T, TypeTraits<T>::kind>::unserialize(*this, in);
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

inline bool isValidVectorType (istream &in, const string &className, bool binary=false)
{
   char ch;
   in >> ch;
   if ((ch == '<' && !binary) || (ch == '{' && binary))
   {
      string type;
      in >> type;
      if (type != "Vector" && type != className)
	 throw new ParsingException ("Parser expected type Vector<Type> and got " + type);
   } else {
      in.putback(ch);
      in.clear(ios::failbit);
      return false;
   }
   return true;
}



template<class T>
istream &operator >> (istream &in, Vector<T> &vec)
{
   if (!isValidVectorType(in, vec.className())) return in;
   vec.readFrom(in);
   return in;
}



#endif
