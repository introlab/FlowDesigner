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
#include "VectorPool.h"

namespace FD {


/**Base class for all vector types, it holds the size and handles some 
   common operations for all vector types.
   @author Jean-Marc Valin
*/
class BaseVector  : public Object {
  protected:

  public:
  
   ///constructor
   BaseVector(){}

   ///return vector size 
   virtual size_t vsize() const = 0;
   
   ///return true if vector empty
   virtual bool vempty() const = 0;

   /** 
   	Returns a new vector (in an ObjectRef) with a range of indexes of the same type.
	\param startInd start index
	\param endInd end indix
	\return new vector in an ObjectRef
   */
   virtual ObjectRef range(size_t startInd, size_t endInd)=0;
      
   /**
   	Returns an element (in an ObjectRef) of the vector
	\param pos the position of the element
	\return ObjectRef the newly created Object
   */
   virtual ObjectRef getIndex(int pos) = 0;
   
   /**
   	Set an element value at the desired position
	\param pos The position in the vector
   	\param val The value to put at the desired position
   */
   virtual void setIndex(int pos, ObjectRef val) = 0;


   /**
      Clone the vector and return an identical copy (deep copy)
   */
   virtual ObjectRef clone() = 0;
   
};

/**The (template) Overflow Vector type, it adds functionnality to the 
   basic vector<T> type, including serialisation and unserialisation
   @author Jean-Marc Valin
*/
template<class T>
class Vector : public BaseVector , public std::vector<T> {
public:

   ///You can always get the type of the Vector elements by using typename Vector<T>::basicType.
   typedef T basicType;

   ///Default constructor, size of the vector is 0.
   Vector()
      : BaseVector()
      , std::vector<T> ()
   {}

   ///Copy constructor
   Vector(const Vector<T> &v)
      : BaseVector()
      , std::vector<T> (v)
   {
   }

   /**
      Constructor with a size and an initialization value.
      \param n the size of the vector
      \param x the initialization value
   */
   explicit Vector(size_t n, const T &x = T())
      : BaseVector()
      , std::vector<T> (n, x)
   {}

   ///Destructor
   ~Vector()
   {
   }
 
   virtual size_t vsize() const {return this->size();}
   
   virtual bool vempty() const {return this->empty();}
   
   /**
      Formatted output (only values) for Vectors <br>
      <b>Format : </b> <i> element0 element1 ... element(size-1)</i>
   */
   void prettyPrint(std::ostream &out=std::cout) const;

   /**
      Formatted output in the FlowDesigner format<br>
      <b>Format : </b> \<Vector\<T\> <i> element0 element1 ... element(size - 1)</i> \>
      \param out the output stream
   */
   void printOn(std::ostream &out) const;
   
   /**
      Formatted input in the FlowDesigner format<br>
      <b>Format : </b> \<Vector\<T\> <i> element0 element1 ... element(size - 1)</i> \>
      \param in the input stream
   */
   void readFrom(std::istream &in=std::cin);

   /**
      Binary output in the FlowDesigner format<br>
      <b>Format : </b> {Vector\<T\> |<i>element0;element1;...; element(size - 1)</i> }
      \param out the output stream
   */
   virtual void serialize(std::ostream &out) const;

   /**
      Binary input in the FlowDesigner format<br>
      <b>Format : </b> {Vector\<T\> |<i>element0;element1;...; element(size - 1)</i> }
      \param in the input stream
   */
   virtual void unserialize(std::istream &in);

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
   static std::string GetClassName()
   {
      std::string name = ObjectGetClassName<Vector<T> >();
      if (name == "unknown")
	 return std::string("Vector");
      //return string("Vector<") + Object::GetClassName<T>() + ">";
      else
	 return name;
   }

   /**
      Returns the class name : Vector<T>
      \return string the class name
   */
   std::string getClassName() {return GetClassName();}

   
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
   virtual ObjectRef getIndex(int pos);

   /**
   	Set an element value at the desired position
	\param pos The position in the vector
   	\param val The value to put at the desired position
   */
   virtual void setIndex(int pos, ObjectRef val);


   /**
      Clone the vector and return an identical copy (deep copy) 
   */
   virtual ObjectRef clone();
};


///clone default implementation
template <class T>
inline ObjectRef Vector<T>::clone() {
  Vector<T> *cpy = Vector<T>::alloc(this->size());
  
  for (unsigned int i = 0; i < this->size(); i++) {
    (*cpy)[i] = (*this)[i];
  }
  return ObjectRef(cpy);
}


///clone implementation with Vector<ObjectRef>
template <>
inline ObjectRef Vector<ObjectRef>::clone() {
  
  Vector<ObjectRef> *cpy = new Vector<ObjectRef>(this->size());
  
  for (unsigned int i = 0; i < this->size(); i++) {
    //cloning every Object in the vector
    (*cpy)[i] = (*this)[i]->clone();
  }

  return ObjectRef(cpy);
}


/*template <class T>
inline std::ostream &operator << (std::ostream &out, const Vector<T> &v)
{
   v.printOn(out);
   return out;
   }*/


template <class T>
inline void _vector_printOn(const Vector<T> &v, std::ostream &out)
{
   out << "<" << v.className();
   for (size_t i=0; i < v.size(); i++)
   {
      out << " " << v[i];
   }
   out << " > ";
}

template <>
inline void _vector_printOn(const Vector<std::string> &v, std::ostream &out)
{
   out << "<Vector<string>";
   for (unsigned int n=0; n < v.size(); n++)
   {
      out << " ";
      const std::string &str = v[n];
      for (unsigned int i=0;i<str.size();i++)
      {
	 if (str[i] == '>')
	 {
	    out.put('\\');
	    out.put('>');
	 } else if (str[i] == ' ')
	 {
	    out.put('\\');
	    out.put(' ');
	 } else if (str[i] == '\\')
	 {
	    out.put('\\');
	    out.put('\\');
	 } else
	    out.put(str[i]);
      }
   }
   out << "> ";
}

template <class T>
inline void _vector_printOn(const Vector<T*> &v, std::ostream &out)
{
   out << "<" << v.className();
   for (size_t i=0; i < v.size(); i++)
   {
      out << " " << *(v[i]);
   }
   out << " > ";
}

template <class T>
void Vector<T>::printOn(std::ostream &out) const
{
   _vector_printOn(*this, out);
}

template <class T>
void Vector<T>::prettyPrint(std::ostream &out) const
{
  _vector_printOn(*this,out);
}


template <class T>
inline void _vector_readFrom(Vector<T> &v, std::istream &in)
{
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
	    //throw new GeneralException("Error reading Vector: '>' expected", __FILE__, __LINE__);
            a=true;
            break;
         }
      }
      T tmp;
      in >> tmp;
      if (in.fail()) 
      {
	 throw new GeneralException("Error reading Vector", __FILE__, __LINE__);
      }
      v.push_back(tmp);
   }
   if (a) 
      throw new GeneralException("Error reading Vector: '>' expected", __FILE__, __LINE__);
}


template <>
inline void _vector_readFrom(Vector<std::string> &v, std::istream &in)
{
   bool done=false;
   while (1)
   {      
      std::string tmp;
      int i=0;
      while(1)
      {
	 char ch;
	 in.get(ch);
	 if (in.eof() || in.fail())
	    throw new GeneralException("Error reading String: '>' or '}' expected", __FILE__, __LINE__);
	 if (ch == '\\')
	 {
	    in.get(ch);
	    tmp += ch;
	 }
	 else if (ch == ' ')
	 {
	    if (i)
	    {
	       break;
	    }
	    else
	       continue;
	 }
	 else if (ch == '>')
	 {
	    done=true;
	    break;
	 }
	 else if (ch == '}')
	 {
	    break;
	 }
	 else
	 {
	    tmp += ch;
	 }
	 i++;
      }

      if (tmp != "")
	 v.push_back(tmp);
      if (done)
	 break;
   }
}

template <class T>
inline void _vector_readFrom(Vector<T*> &v, std::istream &in)
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


template <class T>
inline void Vector<T>::readFrom(std::istream &in)
{
   _vector_readFrom(*this, in);
}

//FIXME: Serialize problems with (Object *)
template<class T, int I>
struct VecMethod {
   static inline void serialize(const Vector<T> &v, std::ostream &out)
   {
     throw new GeneralException("VecMethod default serialize should never be called", __FILE__, __LINE__);
   }
   static inline void unserialize(Vector<T> &v, std::istream &in)
   {
     throw new GeneralException("VecMethod default unserialize should never be called", __FILE__, __LINE__);
   }  
   static inline ObjectRef getIndex(Vector<T> &v, int pos) 
   {
     throw new GeneralException("VecMethod getIndex should never be called", __FILE__, __LINE__);   				
   }   
   static inline void setIndex(Vector<T> &v, int pos, ObjectRef val) 
   {
     throw new GeneralException("VecMethod setIndex should never be called", __FILE__, __LINE__);   				
   }
};

template<class T>
struct VecMethod<T,TTraits::Object> {
   static inline void serialize(const Vector<T> &v, std::ostream &out)
   {
      out << "{" << v.className() << std::endl;
      out << "|";
      int tmp=v.size();
      BinIO::write(out, &tmp, 1);
      for (size_t i=0;i<v.size();i++)
      {
	 v[i].serialize(out);
      }
      out << "}";
   }
   static inline void unserialize(Vector<T> &v, std::istream &in)
   {
      int tmp;
      std::string expected = Vector<T>::GetClassName();
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
  

   static inline ObjectRef getIndex(Vector<T> &v, int pos) 
   {
     if (pos < 0 || pos >= v.size()) {
       throw new GeneralException("Vector getIndex : index out of bound",__FILE__,__LINE__);
     }
     return ObjectRef(v[pos].clone());
   }   

   static inline void setIndex(Vector<T> &v, int pos, ObjectRef val) 
   {
     if (pos < 0 || pos >= v.size()) {
       throw new GeneralException("Vector getIndex : index out of bound",__FILE__,__LINE__);
     }
     RCPtr<T> obj = val;
     v[pos] = *obj;
   }
};

template<class T>
struct VecMethod<T,TTraits::ObjectPointer> {
   static inline void serialize(const Vector<T> &v, std::ostream &out)
   {
      out << "{" << v.className() << std::endl;
      out << "|";
      int tmp=v.size();
      BinIO::write(out, &tmp, 1);
      for (size_t i=0;i<v.size();i++)
      {
	 v[i]->serialize(out);
      }
      out << "}";
   }
   static inline void unserialize(Vector<T> &v, std::istream &in)
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

   static inline ObjectRef getIndex(Vector<T> &v, unsigned int pos) 
   {
     if (pos >= v.size()) {
       throw new GeneralException("Vector getIndex : index out of bound",__FILE__,__LINE__);
     }
     return v[pos];
   }   

   static inline void setIndex(Vector<T> &v, unsigned int pos, ObjectRef val) 
   {
     if (pos >= v.size()) {
       throw new GeneralException("Vector getIndex : index out of bound",__FILE__,__LINE__);
     }
     v[pos] = val;
   }
};


template<class T>
struct VecMethod<T,TTraits::Basic> {
   static inline void serialize(const Vector<T> &v, std::ostream &out)
   {
      out << "{" << v.className() << std::endl;
      out << "|";
      int tmp=v.size();
      BinIO::write(out, &tmp, 1);
      BinIO::write(out, &v[0], v.size());
      out << "}";
   }
   static inline void unserialize(Vector<T> &v, std::istream &in)
   {
      int tmp;
      BinIO::read(in, &tmp, 1);
      v.resize(tmp);
      BinIO::read(in, &v[0], v.size());
      char ch;
      in >> ch;
   }
   
   static inline ObjectRef getIndex(Vector<T> &v, unsigned int pos) 
   {
     if (pos >= v.size()) {
       throw new GeneralException("Vector getIndex : index out of bound",__FILE__,__LINE__);
     }     
     return ObjectRef( NetCType<T>::alloc(v[pos]));
   }   
   
   static inline void setIndex(Vector<T> &v, unsigned int pos, ObjectRef val) 
   {
     if (pos >= v.size()) {
       throw new GeneralException("Vector getIndex : index out of bound",__FILE__,__LINE__);
     }  
     RCPtr<NetCType<T> > obj = val;     
     v[pos] = *obj;   
   }
};

template<class T>
struct VecMethod<T,TTraits::Unknown> {
   static inline void serialize(const Vector<T> &v, std::ostream &out)
   {
      throw new GeneralException(std::string("Sorry, can't serialize this kind of object (") + typeid(T).name()
				 + ")", __FILE__, __LINE__);
   }
   static inline void unserialize(Vector<T> &v, std::istream &in)
   {
      throw new GeneralException(std::string("Sorry, can't unserialize this kind of object (") + typeid(T).name()
				 + ")", __FILE__, __LINE__);
   }

   static inline ObjectRef getIndex(Vector<T> &v, int pos) 
   {
     throw new GeneralException(std::string("Sorry, can't getIndex for this type of vector (") + typeid(T).name()
				 + ")", __FILE__, __LINE__);
   }
   
   static inline void setIndex(Vector<T> &v, int pos, ObjectRef val) 
   {
     throw new GeneralException(std::string("Sorry, can't getIndex for this type of vector (") + typeid(T).name()
				+ ")", __FILE__, __LINE__);
   }
};

template <class T>
inline void Vector<T>::serialize(std::ostream &out) const
{
   VecMethod<T, TypeTraits<T>::kind>::serialize(*this, out);
}

template <class T>
inline void Vector<T>::unserialize(std::istream &in)
{
   VecMethod<T, TypeTraits<T>::kind>::unserialize(*this, in);
}

template <class T>
inline ObjectRef Vector<T>::getIndex(int pos)
{
  return VecMethod<T, TypeTraits<T>::kind>::getIndex(*this,pos);
}

template <class T>
inline void Vector<T>::setIndex(int pos, ObjectRef val)
{
  VecMethod<T, TypeTraits<T>::kind>::setIndex(*this,pos,val);
}


template <class T>
inline void Vector<T>::destroy()
{
   delete this;
}

//#include "VectorPool.h"
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

inline bool isValidVectorType (std::istream &in, std::string type, const std::string &className)
{
   if (type != "Vector" && type != className)
      return false;
   return true;
}


template<class T>
std::istream &operator >> (std::istream &in, Vector<T> &vec)
{
   char ch;
   in >> ch;

   std::string expected = ObjectGetClassName<Vector<T> >();

   if (ch == '<')
   {
      std::string type;
      in >> type;
      if (!isValidVectorType(in, type, expected))
	 throw new ParsingException ("Parser expected type " + expected + " and got " + type);
      vec.readFrom(in);
   } else if (ch == '{')
   {
      std::string type;
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


}//end namespace FD


#endif
