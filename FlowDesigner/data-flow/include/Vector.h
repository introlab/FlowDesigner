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
   void printOn(ostream &out) const;
   
   void readFrom(istream &in=cin);

   virtual void serialize(ostream &out) const;

   virtual void unserialize(istream &in);

   virtual void destroy();

   static Vector<T> *alloc(int size);

   static string GetClassName() 
   {
      string name = ObjectGetClassName<Vector<T> >();
      if (name == "unknown")
	 return string("Vector");
      //return string("Vector<") + Object::GetClassName<T>() + ">";
      else
	 return name;
   }
   string getClassName() {return GetClassName();}
};

/*template <class T>
inline ostream &operator << (ostream &out, const Vector<T> &v)
{
   v.printOn(out);
   return out;
   }*/


template <class T>
inline void _vector_printOn(const Vector<T> &v, ostream &out)
{
   out << "<" << v.className();
   for (unsigned int i=0; i < v.size(); i++)
   {
      out << " " << v[i];
   }
   out << " > ";
}

/*The following code doesn't compile on MSVC++*/
#ifndef BROKEN_TEMPLATES
template <class T>
inline void _vector_printOn(const Vector<T*> &v, ostream &out)
{
   out << "<" << v.className();
   for (unsigned int i=0; i < v.size(); i++)
   {
      out << " " << *(v[i]);
   }
   out << " > ";
}
#endif

template <class T>
void Vector<T>::printOn(ostream &out) const
{
   _vector_printOn(*this, out);
}

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

/*The following code doesn't compile on MSVC++*/
#ifndef BROKEN_TEMPLATES
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

#endif

template <class T>
inline void Vector<T>::readFrom(istream &in)
{
   _vector_readFrom(*this, in);
}

/*The following code requires template partial specialization*/
#ifndef BROKEN_TEMPLATES

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
      for (unsigned int i=0;i<v.size();i++)
      {
	 v[i].serialize(out);
      }
      out << "}";
   }
   static inline void unserialize(Vector<T> &v, istream &in)
   {
      int tmp;
      string expected = Vector<T>::GetClassName();
      BinIO::read(in, &tmp, 1);
      v.resize(tmp);
      for (unsigned int i=0;i<v.size();i++)
      {
	 if (!isValidType(in, expected))
	    throw new ParsingException("Expected type " + expected);
	 v[i].unserialize(in);
      }
      char ch;
      in >> ch;
   }
};

template<class T>
struct VecBinary<T,TTraits::ObjectPointer> {
   static inline void serialize(const Vector<T> &v, ostream &out)
   {
      out << "{" << v.className() << endl;
      out << "|";
      int tmp=v.size();
      BinIO::write(out, &tmp, 1);
      for (unsigned int i=0;i<v.size();i++)
      {
	 v[i]->serialize(out);
      }
      out << "}";
   }
   static inline void unserialize(Vector<T> &v, istream &in)
   {
      int tmp;
      BinIO::read(in, &tmp, 1);
      v.resize(tmp);
      for (unsigned int i=0;i<v.size();i++)
      {
	 in >> v[i];
      }
      char ch;
      in >> ch;
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
      int tmp;
      BinIO::read(in, &tmp, 1);
      v.resize(tmp);
      BinIO::read(in, &(const_cast<Vector<T> &>(v))[0], v.size());
      char ch;
      in >> ch;
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

#else /* #ifndef BROKEN_TEMPLATES */

/* This is for broken compilers */
template<class T, int I>
struct VecBinary {
   static inline void serialize(const Vector<T> &v, ostream &out)
   {
      throw new GeneralException("Binary IO not supported because compiler doesn't support template partial specialization", __FILE__, __LINE__);
   }
   static inline void unserialize(Vector<T> &v, istream &in)
   {
      throw new GeneralException("Binary IO not supported because compiler doesn't support template partial specialization", __FILE__, __LINE__);
   }
};

#endif

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

inline bool isValidVectorType (istream &in, string type, const string &className)
{
   if (type != "Vector" && type != className)
      return false;
   return true;
}


template<class T>
istream &operator >> (istream &in, Vector<T> &vec)
{
   char ch;
   in >> ch;

   string expected = ObjectGetClassName<Vector<T> >();

   if (ch == '<')
   {
      string type;
      in >> type;
      if (!isValidVectorType(in, type, expected))
	 throw new ParsingException ("Parser expected type " + expected + " and got " + type);
      vec.readFrom(in);
   } else if (ch == '{')
   {
      string type;
      in >> type;
      if (!isValidVectorType(in, type, expected))
	 throw new ParsingException ("Parser expected type " + expected + " and got " + type);
      char dummy;
      do {
         in >> dummy;
      } while(dummy != '|');
      vec.unserialize(in);
   } else {
      throw new ParsingException ("Parser expected < or { while parsing type " + expected);
   }
   return in;
}



#endif
