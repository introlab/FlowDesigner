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

/**Base class for all vector types, it holds the size and handles some 
   common operations for all vector types.
   @author Jean-Marc Valin
*/
class BaseVector  : public Object {
  protected:
   size_t obj_size;
   size_t obj_capacity;
  public:
   BaseVector(size_t _obj_size, size_t _obj_capacity)
      : obj_size(_obj_size)
      , obj_capacity(_obj_capacity)
      {}

   size_t vsize() const {return obj_size;}
   
   virtual size_t vector_size() const = 0;

   bool vempty() const {return vsize()==0;}

   size_t vcapacity() {return obj_capacity;}
   
   virtual ObjectRef range(size_t startInd, size_t endInd)=0;
   
   virtual ObjectRef index(int pos) {
   	throw new GeneralException(string("index not implemented for object : ") + className(),__FILE__,__LINE__);
   }
};

/**The (template) Overflow Vector type, it adds functionnality to the 
   basic vector<T> type, including serialisation and unserialisation
   @author Jean-Marc Valin
*/
template<class T>
class Vector : public BaseVector , public vector<T> {
public:

   ///You can always get the type of the Vector elements by using typename Vector<T>::basicType.
   typedef T basicType;

   ///Default constructor, size of the vector is 0.
   Vector()
      : BaseVector(0,0)
      , vector<T> ()
   {}

   ///Copy constructor
   Vector(const Vector<T> &v)
      : BaseVector(v.obj_size,v.obj_size)
      , vector<T> (v)
   {
   }

   /**
      Constructor with a size and an initialization value.
      \param n the size of the vector
      \param x the initialization value
   */
   explicit Vector(size_t n, const T &x = T())
      : BaseVector(0,0)
      , vector<T> (n, x)
   {}

   ///Destructor
   ~Vector()
   {
   }
 
   virtual size_t vector_size() const {return size();}
   
   /**
      Formatted output (only values) for Vectors <br>
      <b>Format : </b> <i> element0 element1 ... element(size-1)</i>
   */
   void prettyPrint(ostream &out=cout) const;

   /**
      Formatted output in the FlowDesigner format<br>
      <b>Format : </b> \<Vector\<T\> <i> element0 element1 ... element(size - 1)</i> \>
      \param out the output stream
   */
   void printOn(ostream &out) const;
   
   /**
      Formatted input in the FlowDesigner format<br>
      <b>Format : </b> \<Vector\<T\> <i> element0 element1 ... element(size - 1)</i> \>
      \param in the input stream
   */
   void readFrom(istream &in=cin);

   /**
      Binary output in the FlowDesigner format<br>
      <b>Format : </b> {Vector\<T\> |<i>element0;element1;...; element(size - 1)</i> }
      \param out the output stream
   */
   virtual void serialize(ostream &out) const;

   /**
      Binary input in the FlowDesigner format<br>
      <b>Format : </b> {Vector\<T\> |<i>element0;element1;...; element(size - 1)</i> }
      \param in the input stream
   */
   virtual void unserialize(istream &in);

   ///destroy() will be called by the vector pool to permanently delete a Vector<T> object
   virtual void destroy();

   /**
      alloc() is called to allocate a vector on the vector pool. The vector, if not used will be
      placed in the vector pool to be reused later.
      \param size size of the vector.
   */
   static Vector<T> *alloc(size_t size);

   /**
      Returns the class name : Vector<T>
      \return string the class name
   */
   static string GetClassName()
   {
      string name = ObjectGetClassName<Vector<T> >();
      if (name == "unknown")
	 return string("Vector");
      //return string("Vector<") + Object::GetClassName<T>() + ">";
      else
	 return name;
   }

   /**
      Returns the class name : Vector<T>
      \return string the class name
   */
   string getClassName() {return GetClassName();}

   
   /**
      Returns a new vector containing data from start to end indexes.
      \param startInd start index
      \param endInd end index
      \return ObjectRef the newly allocated vector 
   */
   ObjectRef range(size_t startInd, size_t endInd)
   {
      Vector<T> *v = Vector<T>::alloc(endInd-startInd+1);
      if (endInd >= v->size() || startInd < 0)
	 throw new GeneralException("Index out of range in BaseVector::range()", __FILE__, __LINE__);
      for (size_t i=startInd;i<=endInd;i++)
      {
	 (*v)[i-startInd]=(*this)[i];
      }
      return ObjectRef(v);
   }
   
   /**	
   	Returns an element at a desired position in the vector
   	\param pos the position in the vector
	\return ObjectRef Element at the desired position
   */
   virtual ObjectRef index(int pos);

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
   for (size_t i=0; i < v.size(); i++)
   {
      out << " " << v[i];
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
   for (size_t i=0; i < v.size(); i++)
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
void Vector<T>::prettyPrint(ostream &out) const
{
  _vector_printOn(*this,out);
}

template <class T>
inline void _vector_readFrom(Vector<T> &v, istream &in)
{
   //cerr << "Reading vector" << endl;
   bool a=false;
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
         {
            //cerr << "Error here!" << endl;
	    //throw new GeneralException("Error reading Vector: '>' expected", __FILE__, __LINE__);
            a=true;
            break;
         }
      }
      T tmp;
      in >> tmp;
      if (in.fail()) 
      {
         //cerr << "Error there" << endl;
	 throw new GeneralException("Error reading Vector", __FILE__, __LINE__);
      }
      v.push_back(tmp);
   }
   if (a) 
      throw new GeneralException("Error reading Vector: '>' expected", __FILE__, __LINE__);
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
      for (size_t i=0;i<v.size();i++)
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
      for (size_t i=0;i<v.size();i++)
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
      for (size_t i=0;i<v.size();i++)
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
      for (size_t i=0;i<v.size();i++)
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
      BinIO::write(out, &v[0], v.size());
      out << "}";
   }
   static inline void unserialize(Vector<T> &v, istream &in)
   {
      int tmp;
      BinIO::read(in, &tmp, 1);
      v.resize(tmp);
      BinIO::read(in, &v[0], v.size());
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
inline Vector<T> *Vector<T>::alloc(size_t size)
{
   return new Vector<T> (size);
}


template <>
inline Vector<float> *Vector<float>::alloc(size_t size)
{
   return floatVectorPool.newVector(size);
}

template <>
inline Vector<double> *Vector<double>::alloc(size_t size)
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


/**************************************************/
/*                 Operators                      */
/**************************************************/

template<class T>
inline Vector<T> &operator+= (Vector<T> &v1, const Vector<T> &v2) 
{
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   for (size_t i=0;i<v1.size();i++)
      v1[i] += v2[i];
   return v1;
}

template<class T>
inline Vector<T> &operator-= (Vector<T> &v1, const Vector<T> &v2) 
{
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   for (size_t i=0;i<v1.size();i++)
      v1[i] -= v2[i];
   return v1;
}

template<class T>
inline Vector<T> &operator*= (Vector<T> &v1, const Vector<T> &v2) 
{
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   for (size_t i=0;i<v1.size();i++)
      v1[i] *= v2[i];
   return v1;
}

template<class T>
inline Vector<T> &operator/= (Vector<T> &v1, const Vector<T> &v2) 
{
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   for (size_t i=0;i<v1.size();i++)
      v1[i] /= v2[i];
   return v1;
}


template<class T>
inline Vector<T> operator+ (const Vector<T> &v1, const Vector<T> &v2) 
{
   Vector<T> v(v1);
   v += v2;
   return v;
}

template<class T>
inline Vector<T> operator- (const Vector<T> &v1, const Vector<T> &v2) 
{
   Vector<T> v(v1);
   v -= v2;
   return v;
}

template<class T>
inline Vector<T> operator* (const Vector<T> &v1, const Vector<T> &v2) 
{
   Vector<T> v(v1);
   v *= v2;
   return v;
}

template<class T>
inline Vector<T> operator/ (const Vector<T> &v1, const Vector<T> &v2) 
{
   Vector<T> v(v1);
   v /= v2;
   return v;
}

template<class T>
inline Vector<T> operator- (Vector<T> &v1) 
{
   Vector<T> v(v1.size());
   for (size_t i=0;i<v1.size();i++)
      v[i] = -v1[i];
   return v;
}


template<class T>
inline Vector<T> &operator+= (Vector<T> &v1, T scal) 
{
   for (size_t i=0;i<v1.size();i++)
      v1[i] += scal;
   return v1;
}

template<class T>
inline Vector<T> &operator-= (Vector<T> &v1, T scal) 
{
   for (size_t i=0;i<v1.size();i++)
      v1[i] -= scal;
   return v1;
}

template<class T>
inline Vector<T> &operator*= (Vector<T> &v1, T scal) 
{
   for (size_t i=0;i<v1.size();i++)
      v1[i] *= scal;
   return v1;
}

template<class T>
inline Vector<T> &operator/= (Vector<T> &v1, T scal) 
{
   for (size_t i=0;i<v1.size();i++)
      v1[i] /= scal;
   return v1;
}


template<class T>
inline Vector<T> operator+ (const Vector<T> &v1, T scal) 
{
   Vector<T> v(v1);
   v += scal;
   return v;
}

template<class T>
inline Vector<T> operator- (const Vector<T> &v1, T scal) 
{
   Vector<T> v(v1);
   v -= scal;
   return v;
}

template<class T>
inline Vector<T> operator* (const Vector<T> &v1, T scal) 
{
   Vector<T> v(v1);
   v *= scal;
   return v;
}

template<class T>
inline Vector<T> operator/ (const Vector<T> &v1, T scal) 
{
   Vector<T> v(v1);
   v /= scal;
   return v;
}


/*T operator* (const Vector<T> &v2) 
{
   if (size() != v2.size())
      cerr << "Vector size mismatch\n";
   T sum=0;
   for (int i=0;i<size();i++)
      sum += operator[](i)*v2[i];
   return sum;
   }*/



#endif
