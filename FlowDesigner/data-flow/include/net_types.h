#ifndef _NETTYPES_H_
#define _NETTYPES_H_

#include "Object.h"
#include <iostream.h>
#include <string>

/** We must define network types that we want to use in our network.
    We will use a template class where all the proper operators are defined. */

/** This is a generic type that the network will handle. Subclass GenericType if
    you want to add specific operations and operators */
template <class T> 
class GenericType : public Object {

protected:
   T value;

public:

   ///cast operator
   /*operator T() {
      return value;
      }*/

   T &val() {return value;}
   
   ///default constructor
   GenericType() {
      //value = (T) 0;
   }

   ///destructor
   virtual ~GenericType() {;}
   
   ///constructor with a value
   GenericType (T val) {
      value = val;
   }
   
   ///copy constructor
   GenericType (GenericType<T> &copy) {
      value = copy.value;
   }

};

/**
   The NetCType. We are using this class to wrap standard C types into objects
   tha are Object compatible.
   @autor Dominic Letourneau
   @version 1.0
 */
template <class T>
class NetCType : public GenericType<T> {
   
public:
   
   ///default constructor
   NetCType() {
      //casting into the type
      GenericType<T>::value = (T) 0;
   }

   ///constructor with a predefined value
   NetCType(T val) { 
      GenericType<T>::value = val;
   }
   
   ///destructor
   virtual ~NetCType() {;}
   
   ///cast operator
   operator T() {
      return GenericType<T>::value;
   }

   ///operator= between a NetCType and another NetCType
   NetCType<T>& operator= (NetCType<T> &type) {
      GenericType<T>::value = type.value;
      return *this;
   }

   ///operator= between a standard C type and a NetCType
   NetCType<T>& operator= (T val) {
      GenericType<T>::value = val;
      return *this;
   }

   ///operator== between two NetCType
   int operator== (NetCType<T> &type) {
      return (type.value == GenericType<T>::value);
   }
   
   ///operator== between a NetCType and a standard C type
   int operator== (T val) {
      return (GenericType<T>::value == val);
   }
   
   ///operator!= between two NetCType
   int operator!= (NetCType<T> &type) {
      return (type.value != GenericType<T>::value);
   }
   
   ///operator!= between a NetCType and a standard C type
   int operator!= (T val) {
      return (GenericType<T>::value != val);
   }
};


///defining the standard C types
//@name type definitions
//@{
typedef NetCType<char> Char;
typedef NetCType<int> Int;
typedef NetCType<short> Short;
typedef NetCType<float> Float;
typedef NetCType<double> Double;
typedef NetCType<long> Long;
typedef NetCType<unsigned char> U_char;
typedef NetCType<unsigned int> U_int;
typedef NetCType<unsigned long> U_long;
typedef NetCType<bool> Bool;

///STL types
typedef GenericType<string> String;
//@}

#endif
