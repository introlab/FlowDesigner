#ifndef _COMPLEX_H_
#define _COMPLEX_H_


#include "Object.h"
#include <iostream>
#include "ObjectPool.h"
#include "ObjectParser.h"
#include "typetraits.h"
#include <complex>
#include "binio.h"

using namespace std;

template <class T>
class Complex : public complex<T>, public Object {

 public:

  typedef complex<T> basicType;
  
  Complex() : complex<T>() {}
  
  Complex(const complex<T> &val) : complex<T>(val) {}
		  
 void printOn(ostream &out) const {
    out << "<"<<className()<<" ";
    out << *((complex<T>*) this);
    out << ">"<<endl;
  }
  
  void readFrom(istream &in) {
    complex<T> value;
    in >> value;
    *this = value;
    char ch;
    in >> ch;
    if (ch != '>') {
      throw new GeneralException(string("Error reading ") + className() + string(" > expected"), __FILE__, __LINE__);
    }
  }
  
  void serialize(ostream &out) const {
    out << "{" << className() << " |";
    BinIO::write(out, (complex<T>*) this, 1);
    out << " }";
  }
  
  void unserialize(istream &in) { 
    complex<T> value;
    BinIO::read(in, &value, 1);
    *this = value;    
    char ch;
    in >> ch;
    if (ch != '}') {
       throw new GeneralException(string("Error reading ") + className() + string(" } expected"), __FILE__, __LINE__);
    }    
  }
  
  void prettyPrint(ostream &out) const { 
    out << *((complex<T>*) this);
  }
  
  complex<T>& val() const {return *((complex<T>*) this);}


   static Complex<T> *alloc()  {return ObjectPool<Complex<T> >::alloc();}

   static Complex<T> *alloc(const Complex<T> &obj)  
   {
      Complex<T> *ret = ObjectPool<Complex<T> >::alloc();
      *ret = obj;
      return ret;
   }

   void destroy() {ObjectPool<Complex<T> >::release(this);}


};

#endif
