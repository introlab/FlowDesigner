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

   ///The value wrapped by a GenericType<T>
   T value;

public:
  
   ///You can always get the type wrapped by GenericType<T> by using typename GenericType<T>::basicType.
   typedef T basicType;

   ///Returns the value
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

   /**
      Formatted output in the FlowDesigner format. Not very useful at this stage.<br>
      <b>Format : </b> \<Generic Type: typeid(T).name() \>
      \param out the output stream
   */
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

   virtual void prettyPrint(ostream &out=cout) const {
     out << value << " ";
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
   that are Object compatible.
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
class String : public string, public Object
{
public:
  typedef string basicType;

   String() : string() {}
   void printOn(ostream &out) const;
   void readFrom(istream &in);
   void serialize(ostream &out) const;
   void unserialize(istream &in);
   void prettyPrint(ostream &out) const;

   String(const char *str) : string(str)
   {}
   String(const string &str) : string(str)
   {}
   
   const string& val() { return *this;}
};

istream &operator >> (istream &in, String &str);

_DEF_OBJECT_TYPE(String)





#endif
