#ifndef _COMPLEX_H_
#define _COMPLEX_H_


#include "Object.h"
#include <iostream>
#include "ObjectPool.h"
#include "ObjectParser.h"
#include "typetraits.h"
#include <complex>
#include "binio.h"
#include "net_types.h"

namespace FD {

/**
   Base class for Complex<T> numbers.
   \author Dominic Letourneau
   \date 18/02/2004
*/
template <class T>
class Complex : public std::complex<T>, public Object {

 public:

  ///You can always get the type of the Complex<T> by using typename Complex<T>::basicType.
  typedef std::complex<T> basicType;
  
  ///Default constructor
  Complex() : std::complex<T>() {}
  
  ///Constructor with a complex<T>
  Complex(const std::complex<T> &val) : std::complex<T>(val) {}

  ///Constructor with a Complex<T>
  Complex(const Complex<T> &val) : std::complex<T>(val) {}

  ///Constructor with a NetCType<complex<T> >
  Complex(const NetCType<std::complex<T> > &obj) : std::complex<T>(obj.val()) {}

  /**
     Formatted output in the FlowDesigner format<br>
     <b>Format : </b> \<Complex\<T\> <i> complex<T> value</i> \>
     \param out the output stream
  */
  void printOn(std::ostream &out) const {
    out << "<"<<className()<<" ";
    out << *((std::complex<T>*) this);
    out << ">"<<std::endl;
  }
  
  /**
     Formatted input in the FlowDesigner format<br>
     <b>Format : </b> \<Complex\<T\> <i> complex<T> value</i> \>
     \param in the input stream
  */
  void readFrom(std::istream &in) {
    std::complex<T> value;
    in >> value;
    *this = value;
    char ch;
    in >> ch;
    if (ch != '>') {
      throw new GeneralException(std::string("Error reading ") + className() + std::string(" > expected"), __FILE__, __LINE__);
    }
  }
  
  /**
      Binary output in the FlowDesigner format<br>
      <b>Format : </b> {Complex\<T\> |<i>complex<T> value</i> }
      \param out the output stream
  */
  void serialize(std::ostream &out) const {
    out << "{" << className() << " |";
    BinIO::write(out, (std::complex<T>*) this, 1);
    out << " }";
  }
  
  /**
      Binary input in the FlowDesigner format<br>
      <b>Format : </b> {Complex\<T\> |<i>complex<T> value</i> }
      \param in the input stream
  */
  void unserialize(std::istream &in) { 
    std::complex<T> value;
    BinIO::read(in, &value, 1);
    *this = value;    
    char ch;
    in >> ch;
    if (ch != '}') {
       throw new GeneralException(std::string("Error reading ") + className() + std::string(" } expected"), __FILE__, __LINE__);
    }    
  }
  
  /**
     Standard formatted output for Complex<T>
     \param out the output stream
  */
  void prettyPrint(std::ostream &out) const { 
    out << *((std::complex<T>*) this);
  }
  
  /**
     Returns the complex<T> wrapped value
     \return complex<T> the complex<T> value wrapped
  */
  std::complex<T>& val() const {return *((std::complex<T>*) this);}
  
  
  /**
     Allocate a Complex<T> value from the pool
     \return Complex<T>* the value from the pool
  */
  static Complex<T> *alloc()  {return ObjectPool<Complex<T> >::alloc();}
  
  /**
     Allocat a Complet<T> value from the pool, copying from another Complex<T> value
     \param obj The value to be copied
     \return Complex<T>* a copy of obj
  */
  static Complex<T> *alloc(const Complex<T> &obj)  
  {
    Complex<T> *ret = ObjectPool<Complex<T> >::alloc();
    *ret = obj;
    return ret;
  }

  /**
     Destroy any Complex<T> from memory. This function is used by the ObjectPool<Complex<T> > class.
  */
  void destroy() {ObjectPool<Complex<T> >::release(this);}

  /**
     clone a Complex<T>
  */
  virtual ObjectRef clone() {
    return ObjectRef(Complex<T>::alloc(*(std::complex<T>*) this));
  }

};

/**
   operator >> for Complex<T>
   \param in input stream
   \param value the Complex<T> value to print
   \return istream the input stream
*/
template <class T> 
std::istream &operator >> (std::istream &in, Complex<T> &value) { 
  
  char ch;
  in >> ch;
  
  std::string expected = ObjectGetClassName<Complex<T> >();
  
  if (ch == '<') {
    std::string type;
    in >> type;
    
    if (expected != type) {
      throw new ParsingException ("Parser expected type " + expected + " and got " + type);
    }

    //reading object
    value.readFrom(in);
  }
  else if (ch == '{') {
    std::string type;
    in >> type;

    if (expected != type) {
      throw new ParsingException ("Parser expected type " + expected + " and got " + type);
    }
    
    //reading dummy spaces
    char dummy;      
    do {
      in >> dummy;
    } while(dummy != '|');

    value.unserialize(in);

  } else {
    throw new ParsingException ("Parser expected < or { while parsing type " + expected);
  }

  return in;
}

_DEF_OBJECT_TYPE(Complex<float>)
_DEF_OBJECT_TYPE(Complex<double>)

}//namespace FD
#endif
