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
//temporarily inserting include of Stream.h
#include "Stream.h"



/** This is a generic type that FlowDesigner will handle. Subclass GenericType if
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
   virtual void printOn(std::ostream &out=std::cout) const {
      out << "<Generic Type: " << typeid(T).name() << ">" << endl;
   }

};

/** 
    This is a generic printable type that FlowDesigner will handle. Subclass PrintableGenericType if
    you want to add specific operations and operators 
*/
template <class T>
class PrintableGenericType : public GenericType<T> {

public:

   /**Default constructor*/
   PrintableGenericType () {}

   /**
      Constructor with a value
      \param val the value
   */
   PrintableGenericType (T val) {
      GenericType<T>::value = val;
   }

   ///copy constructor
   PrintableGenericType (PrintableGenericType<T> &copy) {
      GenericType<T>::value = copy.value;
   }

   /**
      Value accessor.
      \return T the value.
   */
   T &val() {return GenericType<T>::value;}

   /**
      Formatted output in the FlowDesigner format. <br>
      <b>Format : </b> \<T <i>value<\i> \>
      \param out the output stream
   */
   void printOn(std::ostream &out) const
   {
      out << "<" << this->className() << " " << this->value << " >";
   }

   /**
      Formatted output (std)
      \param out output stream
   */
   virtual void prettyPrint(std::ostream &out=std::cout) const {
     out << this->value << " ";
   }

   /**
      Formatted input in the FlowDesigner format. <br>
      <b>Format : </b> \<T <i>value<\i> \>
      \param in the input stream
   */
   void readFrom(std::istream &in)
   {
      in >> this->value;
      char ch;
      in >> ch;
      if (ch != '>')
	 throw new GeneralException("Error reading String: '>' expected", __FILE__, __LINE__);
   }

   /**
      Binary output in the FlowDesigner format. <br>
      <b>Format : </b> {T |<i>value<\i> }
      \param out the output stream
   */
   void serialize(std::ostream &out) const
   {
      out << "{" << this->className() << " |" << this->value << " }";
   }

   /**
      Binary input in the FlowDesigner format. <br>
      <b>Format : </b> {T |<i>value<\i> }
      \param in the input stream
   */
   void unserialize(std::istream &in)
   {
      in >> this->value;
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
   \author Dominic Letourneau & Jean-Marc Valin
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

   /**
      Allocator from the pool
      \return NetCType<T>* The newly created object
   */
   static NetCType<T> *alloc()  {return ObjectPool<NetCType<T> >::alloc();}

   /**
      Allocator from the pool with an object
      \return NetCType<T>* The newly created object (copy of obj)
   */
   static NetCType<T> *alloc(const T &obj)  
   {
      NetCType<T> *ret = ObjectPool<NetCType<T> >::alloc();
      ret->value = obj;
      return ret;
   }

   ///Destroy object from the pool
   void destroy() {ObjectPool<NetCType<T> >::release(this);}

};

///Char which is a NetCType<char>
typedef NetCType<char> Char;

///Int which is a NetCType<int>
typedef NetCType<int> Int;

///Float which is a NetCType<float>
typedef NetCType<float> Float;

///Double which is a NetCType<double>
typedef NetCType<double> Double;

///Bool which is a NetCType<double>
typedef NetCType<bool> Bool;

//typedef NetCType<FILE *> FILEPTR;
/** FILE* wrapper */
class FILEPTR : public GenericType<FILE *> {
  public: 
   FILEPTR(FILE *file);
   FILEPTR(const std::string &filename, const std::string &mode);
   ~FILEPTR();
};

#ifndef WIN32

//typedef NetCType<FILE *> FILEPTR;
/** File descriptor wrapper */
class FILEDES : public GenericType<int> {
  public: 
   FILEDES(int fd);
   FILEDES(const std::string &filename, int mode);
   ~FILEDES();
};

#endif /*ifdef WIN32*/

///TrueObject = NetCType<Bool>(true)
extern ObjectRef TrueObject;

///FalseObject = NetCType<Bool>(false)
extern ObjectRef FalseObject;


/**Base FlowDesigner String type, wraps a C++ string
   \author Jean-Marc Valin
*/
class String : public std::string, public Object
{
 public:

   ///You can always get the type wrapped by String by using typename String::basicType.
   typedef std::string basicType;
   
   /** Default constructor */
   String() : std::string() {}

   /**
      Formatted output in the FlowDesigner format for String.
      <b>Format : </b> \<String <i>the_string_with_escape_characters<\i>\>
      \param out the output stream
   */
   void printOn(std::ostream &out) const;

   /**
      Formatted input in the FlowDesigner format for String.
      <b>Format : </b> \<String <i>the_string_with_escape_characters<\i>\>
      \param in the input stream
   */
   void readFrom(std::istream &in);

   /**
      Binary output in the FlowDesigner String format. <br>
      <b>Format : </b> {String |<i>the_string<\i> }
      \param out the output stream
   */
   void serialize(std::ostream &out) const;

   /**
      Binary input in the FlowDesigner String format. <br>
      <b>Format : </b> {String |<i>the_string<\i> }
      \param in the input stream
   */
   void unserialize(std::istream &in);

   /** 
       Std formatted output
       \param out the output stream
   */
   void prettyPrint(std::ostream &out) const;

   /** Constructor with char* */
   String(const char *str) : std::string(str)
   {}

   /** Constructor with string */
   String(const std::string &str) : std::string(str)
   {}
   
   /** return the string value */
   const std::string& val() { return *this;}
};

///operator >> for String
std::istream &operator >> (std::istream &in, String &str);

_DEF_OBJECT_TYPE(String)

#endif
