// Copyright (C) 1999 Jean-Marc Valin

#ifndef GEN_TYPE_VECTOR_H
#define GEN_TYPE_VECTOR_H

#include "Object.h"
#include <vector>
#include "ObjectParser.h"
#include "ObjectRef.h"
#include "binio.h"
#include "typetraits.h"
#include <string>

using namespace std;

/**The (template) Overflow Vector type, it adds functionnality to the 
   basic vector<T> type, including serialisation and unserialisation
   @author Jean-Marc Valin
*/
template<class T>
class Vector : public Object
{
public:
   typedef T* iterator;
   typedef const T* const_iterator;
protected:
  public:
   T *data;
   size_t obj_size;
   int capacity;
public:
   Vector()
      : data(NULL)
      , obj_size(0)
      , capacity(0)
   {}

   explicit Vector(size_t n, const T &x = T())
      : data(NULL)
      , obj_size(0)
      , capacity(0)
   {resize(n,x);}

   ~Vector()
   {
      if (data)
      {
	 for (iterator i=begin();i!=end();i++)
	    destr(i);
	 delete[] (char *)(data);
	 //free (data);
	 data = NULL;
      }      
   }

   inline void resize(int new_size, const T &x = T());

   iterator begin() {return data;}

   const_iterator begin() const {return data;}

   iterator end() {return data+obj_size;}

   const_iterator end() const {return data+obj_size;}

   size_t size() const {return obj_size;}

   T &operator[] (size_t i) {return data[i];}

   const T &operator[] (size_t i) const {return data[i];}

   void push_back(const T &x) {insert(end(),x);}

   void push_front(const T &x) {insert(begin(),x);}

   inline void insert(iterator after, const T &x);

   inline void erase(iterator item);

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

  protected:
   void constr(T* ptr, const T& val) { new(ptr) T(val);}
   void destr(T* ptr) { ptr->~T(); }

};


template<class T>
void Vector<T>::resize(int new_size, const T &x)
{
   if (new_size > obj_size)
   {
      if (new_size > capacity)
      {
	 //FIXME: This could be optimized a bit...
	 int new_capacity = new_size;

	 T* tmp = (T*)new char [new_capacity*sizeof(T)];
#if 0 //FIXME: Help! Which one is right?
	 for (int i=0;i<obj_size;i++)
	    constr(tmp+i,data[i]);
	 if (data)
	 {
	    for (iterator i=begin();i!=end();i++)
	       destr(i);
	    delete[] (char *)(data);
	 }
#else
	 memcpy(tmp, data, obj_size*sizeof(T));
	 if (data)
	    delete[] (char *)(data);
#endif
	 data = tmp;
	 capacity = new_capacity;
      }

      for (int i=obj_size;i<new_size;i++)
	 constr(data+i,x);

   } else {

      for (int i=new_size;i<obj_size;i++)
	 destr(data+i);

   }
   obj_size = new_size;
}

template<class T>
void Vector<T>::insert(iterator after, const T &x)
{
   int pos = after-begin();
   if (obj_size+1 > capacity)
   {
      //FIXME: This could be optimized a bit...
      int new_capacity = obj_size+1;
      //Allocate new memory
      T* tmp = (T*)new char [new_capacity*sizeof(T)];

#if 0 //FIXME: Help! Which one is right?
      //copy the elements before the inserted object
      for (int i=0;i<pos;i++)
	 constr(tmp+i,data[i]);
      //copy inserted object
      constr(tmp+pos,x);
      //copy elements after insertion
      for (int i=pos+1;i<obj_size+1;i++)
	 constr(tmp+i,data[i-1]);
      //Free old memory
      data = tmp;
      if (data)
      {
	 for (iterator i=begin();i!=end();i++)
	    destr(i);
	 delete[] (char *)(data);
      }
#else
      memcpy(tmp, data, pos*sizeof(T));
      constr(tmp+pos,x);
      memcpy(tmp+pos+1, data+pos, (end()-after)*sizeof(T));
      if (data)
	 delete[] (char *)(data);
      data = tmp;
#endif
      capacity = new_capacity;
   } else {
#if 0 //FIXME: Help! Which one is right?
      for (iterator i=after+1;i<end()+1;i++)
      {
	 constr(i,*(i-1));
	 destr(i-1);
      }
      constr(after,x);
#else
      memmove(after+1, after, (end()-after)*sizeof(T));
      constr(after,x);
#endif
   }
   obj_size++;
}

template<class T>
void Vector<T>::erase(iterator it)
{
#if 0 //FIXME: Help! Which one is right?
   for (iterator i=it;i<end()-1;i++)
   {
      destr(i);
      constr(i,*(i+1));
   }
#else
   memmove(it+1, it, (end()-it-1)*sizeof(T));
#endif
   destr(end()-1);
   obj_size--;
}

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
      T f=v.data[i];
      out << " " << f;
      //out << " " << v[i];
   }
   out << " > ";
}

template <>
inline void _vector_printOn(const Vector<string> &v, ostream &out);

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
   v.resize(0);
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

template <>
inline void _vector_readFrom(Vector<string> &v, istream &in);

/*The following code doesn't compile on MSVC++*/
#ifndef BROKEN_TEMPLATES
template <class T>
inline void _vector_readFrom(Vector<T*> &v, istream &in)
{
   v.resize(0);
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
