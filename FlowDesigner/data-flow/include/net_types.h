// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#ifndef _NETTYPES_H_
#define _NETTYPES_H_


#include "Object.h"
#include <iostream>
#include <string>
#include <fstream>
#include <stdio.h>
#include "ObjectPool.h"
#include "ObjectParser.h"
#include "typetraits.h"

using namespace std;

/** This is a generic type that Overflow will handle. Subclass GenericType if
    you want to add specific operations and operators */
template <class T> 
class GenericType : public Object {

protected:
   T value;

public:
   typedef T basicType;

   T &val() {return value;}
   
   /**default constructor*/
   GenericType() {
      //value = (T) 0;
   }

   /**destructor*/
   virtual ~GenericType() {;}
   
   /**constructor with a value*/
   GenericType (T val) {
      value = val;
   }
   
   /**copy constructor*/
   GenericType (GenericType<T> &copy) {
      value = copy.value;
   }

   //static GenericType<T> *alloc()  {return ObjectPool<GenericType<T> >::alloc();}

   virtual void printOn(ostream &out=cout) const {
      out << "<Generic Type: " << typeid(T).name() << ">" << endl;
   }

};

template <class T>
class PrintableGenericType : public GenericType<T> {

public:
   PrintableGenericType () {}

   PrintableGenericType (T val) {
      GenericType<T>::value = val;
   }

   ///copy constructor
   PrintableGenericType (PrintableGenericType<T> &copy) {
      GenericType<T>::value = copy.value;
   }

   T &val() {return GenericType<T>::value;}

   void printOn(ostream &out) const
   {
      out << "<" << className() << " " << value << " >";
   }
   void readFrom(istream &in)
   {
      in >> value;
      char ch;
      in >> ch;
      if (ch != '>')
	 throw new GeneralException("Error reading String: '>' expected", __FILE__, __LINE__);
   }
   void serialize(ostream &out) const
   {
      out << "{" << className() << " |" << value << " }";
   }
   void unserialize(istream &in)
   {
      in >> value;
      char ch;
      in >> ch;
      if (ch != '}')
	 throw new GeneralException("Error reading String: '}' expected", __FILE__, __LINE__);
   }

   //static PrintableGenericType<T> *alloc()  {return ObjectPool<PrintableGenericType<T> >::alloc();}
};

/**
   The NetCType. We are using this class to wrap standard C types into objects
   tha are Object compatible.
   @author Dominic Letourneau & jean-Marc Valin
 */
template <class T>
class NetCType : public PrintableGenericType<T> {
   
public:
   
   /**default constructor*/
   NetCType() {
      //casting into the type
      GenericType<T>::value = (T) 0;
   }

   /**constructor with a predefined value*/
   NetCType(T val) { 
      GenericType<T>::value = val;
   }
   
   /**destructor*/
   virtual ~NetCType() {;}
   
   /**cast operator*/
   operator T() {
      return GenericType<T>::value;
   }

   /**operator= between a NetCType and another NetCType*/
   NetCType<T>& operator= (NetCType<T> &type) {
      GenericType<T>::value = type.value;
      return *this;
   }

   /**operator= between a standard C type and a NetCType*/
   NetCType<T>& operator= (T val) {
      GenericType<T>::value = val;
      return *this;
   }

   /**operator== between two NetCType*/
   int operator== (NetCType<T> &type) {
      return (type.value == GenericType<T>::value);
   }
   
   /**operator== between a NetCType and a standard C type*/
   int operator== (T val) {
      return (GenericType<T>::value == val);
   }
   
   /**operator!= between two NetCType*/
   int operator!= (NetCType<T> &type) {
      return (type.value != GenericType<T>::value);
   }
   
   /**operator!= between a NetCType and a standard C type*/
   int operator!= (T val) {
      return (GenericType<T>::value != val);
   }

   static NetCType<T> *alloc()  {return ObjectPool<NetCType<T> >::alloc();}

   static NetCType<T> *alloc(const T &obj)  
   {
      NetCType<T> *ret = ObjectPool<NetCType<T> >::alloc();
      ret->value = obj;
      return ret;
   }

   void destroy() {ObjectPool<NetCType<T> >::release(this);}

};


typedef NetCType<int> Int;
typedef NetCType<float> Float;
typedef NetCType<double> Double;
typedef NetCType<bool> Bool;


//typedef NetCType<FILE *> FILEPTR;
class FILEPTR : public GenericType<FILE *> {
  public: 
   FILEPTR(FILE *file);
   FILEPTR(const string &filename, const string &mode);
   ~FILEPTR();
};

#ifndef WIN32

//typedef NetCType<FILE *> FILEPTR;
class FILEDES : public GenericType<int> {
  public: 
   FILEDES(int fd);
   FILEDES(const string &filename, int mode);
   ~FILEDES();
};

#endif /*ifdef WIN32*/


extern ObjectRef TrueObject;
extern ObjectRef FalseObject;


/**Base Overflow String type, wraps a C++ string
   @author Jean-Marc Valin
*/
class String : virtual public string, virtual public Object
{
public:
   String() : string() {}
   void printOn(ostream &out) const
   {
      out << "<String " << *(string*) (this) << " >";
   }
   void readFrom(istream &in)
   {
      in >> (*(string*) (this));
      char ch;
      in >> ch;
      if (ch != '>')
	 throw new GeneralException("Error reading String: '>' expected", __FILE__, __LINE__);
   }
   void serialize(ostream &out) const
   {
      out << "{String |" << *(string*) (this) << " }";
   }
   void unserialize(istream &in)
   {
      in >> (*(string*) (this));
      char ch;
      in >> ch;
      if (ch != '}')
	 throw new GeneralException("Error reading String: '}' expected", __FILE__, __LINE__);
   }
   String(const char *str) : string(str)
   {}
   String(const string &str) : string(str)
   {}
   
};


_DEF_OBJECT_TYPE(String)


/**Base Overflow Stream type, wraps a C++ stream
   @author Jean-Marc Valin
*/
class Stream : public Object
{
  protected:
   ios *int_stream;
   bool owner;
  public:
   Stream(ios *_str, bool _owner=true)
      : int_stream(_str)
      , owner(_owner)
      {}
   virtual void printOn(ostream &out) const {out << "<Stream>";}
   int eof() {return int_stream->eof();}
   int fail() {return int_stream->fail();}
   void flush() {dynamic_cast<ostream *> (int_stream)->flush();}
   
   Stream &read (char *ch, int len) {dynamic_cast<istream *> (int_stream)->read(ch,len); return *this;}
   Stream &write (const char *ch, int len) {dynamic_cast<ostream *> (int_stream)->write(ch,len); return *this;}
   Stream &getline (char *ch, int len) {dynamic_cast<istream *> (int_stream)->getline(ch,len); return *this;}
   Stream &seekg (int pos, ios::seekdir dir) {dynamic_cast<istream *> (int_stream)->seekg(pos, dir); return *this;}
   Stream &seekp (int pos, ios::seekdir dir) {dynamic_cast<ostream *> (int_stream)->seekp(pos, dir); return *this;}

   operator istream &() {return *dynamic_cast<istream *> (int_stream);}
   operator ostream &() {return *dynamic_cast<ostream *> (int_stream);}
   operator iostream &() {return *dynamic_cast<iostream *> (int_stream);}
   
   template <class T>
      Stream &operator >> (T &obj) {*dynamic_cast<istream *> (int_stream) >> obj; return *this;}
   template <class T>
      Stream &operator << (T &obj) {*dynamic_cast<ostream *> (int_stream) << obj; return *this;}
   virtual ~Stream() {}
   

};

class IStream : virtual public Stream {
  public:
   IStream(istream *_str, bool _owner=true)
      : Stream(_str, _owner)
      {}
   void printOn(ostream &out) const {out << "<IStream unknown>";}
   ~IStream() {if (owner) delete dynamic_cast<istream *>(int_stream);}

};

class OStream : virtual public Stream {
  public:
   OStream(ostream *_str, bool _owner=true)
      : Stream(_str, _owner)
      {}
   void printOn(ostream &out) const {out << "<OStream unknown>";}
   ~OStream() {if (owner) delete dynamic_cast<ostream *>(int_stream);}

};

class IOStream : public IStream, public OStream {
  public:
   IOStream(iostream *_str, bool _owner=true)
      : Stream(_str, _owner)
      , IStream(_str, _owner)
      , OStream(_str, _owner)
      {}
   void printOn(ostream &out) const {out << "<IOStream unknown>";}
   ~IOStream() {if (owner) delete dynamic_cast<iostream *>(int_stream);}

};




#endif
