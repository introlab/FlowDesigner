// Copyright (C) 2001 Jean-Marc Valin

#include "Vector.h"
#include "Object.h"
#include "ObjectParser.h"
#include "VectorPool.h"

#include "ObjectRef.h"
#include "DoubleDispatch.h"
#include "operators.h"
#include "vec.h"
#include <complex>
#include "Complex.h"

//@implements core

//static int dummy = Object::addObjectType<Vector<float> > ("Vector", new ObjectFactory<Vector<float> > ("Vector"));
DECLARE_TYPE2("Vector", Vector<float>)
DECLARE_TYPE(Vector<float>)
DECLARE_TYPE(Vector<double>)
DECLARE_TYPE(Vector<int>)
//DECLARE_TYPE(Vector<bool>)
DECLARE_TYPE(Vector<ObjectRef>)
DECLARE_TYPE2("Vector<complex<float>>", Vector<complex<float> >)
DECLARE_TYPE2("Vector<complex<double>>", Vector<complex<double> >)
DECLARE_TYPE(Vector<string>)
DECLARE_TYPE(Vector<String>)

VectorPool<float> floatVectorPool;
VectorPool<double> doubleVectorPool;

//pretty print specialization

void Vector<float>::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<endl;
}

void Vector<double>::prettyPrint(ostream &out) const {
 for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<endl;
}

void Vector<int>::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<endl;
}

#if 0
void Vector<bool>::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<endl;
}
#endif

void Vector<ObjectRef>::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    (*this)[i]->prettyPrint(out);
    out<<endl;
  } 
}

void Vector<complex<float> >::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<endl;
}

void Vector<complex<double> >::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<endl;
}


void Vector<string>::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<endl;
  }
}

void Vector<String>::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<endl;
  }
}

template <>
inline void _vector_printOn(const Vector<string> &v, ostream &out)
{
   out << "<Vector<string>";
   for (unsigned int n=0; n < v.size(); n++)
   {
      out << " ";
      const string &str = v[n];
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

template <>
inline void _vector_readFrom(Vector<string> &v, istream &in)
{
   bool done=false;
   while (1)
   {
      
      string tmp;
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

/*
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
      string tmp;
      in >> tmp;
      if (in.fail()) 
	 throw new GeneralException("Error reading Vector", __FILE__, __LINE__);
      v.push_back(tmp);
*/

   }

}

//getIndex implementation for Vector<bool>
ObjectRef Vector<bool>::getIndex(int pos) {
	//look for range ?
	return ObjectRef(Bool::alloc(static_cast<basicType> ((*this)[pos])));
}

//getIndex implementation for Vector<int>
ObjectRef Vector<int>::getIndex(int pos) {
	//look for range ?
	return ObjectRef(Int::alloc(static_cast<basicType> ((*this)[pos])));
}

//getIndex implementation for Vector<float>
ObjectRef Vector<float>::getIndex(int pos) {
	//look for range ?
	return ObjectRef(Float::alloc(static_cast<basicType> ((*this)[pos])));
}

//getIndex implementation for Vector<double>
ObjectRef Vector<double>::getIndex(int pos) {
	//look for range ?
	return ObjectRef(Double::alloc(static_cast<basicType> ((*this)[pos])));
}

//getIndex implementation for Vector<complex<float> >
ObjectRef Vector<complex<float> >::getIndex(int pos) {
	//look for range ?
	return ObjectRef(Complex<float>::alloc(static_cast<basicType> ((*this)[pos])));
}

//getIndex implementation for Vector<complex<double> >
ObjectRef Vector<complex<double> >::getIndex(int pos) {
	//look for range ?
	return ObjectRef(Complex<double>::alloc(static_cast<basicType> ((*this)[pos])));
}

//getIndex implementation for Vector<ObjectRef>
ObjectRef Vector<ObjectRef>::getIndex(int pos) {
	//look for range ?
	return ObjectRef((*this)[pos]);
}

//getIndex implementation for Vector<String>
ObjectRef Vector<String>::getIndex(int pos) {
	//look for range ?
	String *new_string = new String();
	*new_string = (*this)[pos];
	return ObjectRef(new_string);
}

//getIndex implementation for Vector<string>
ObjectRef Vector<string>::getIndex(int pos) {
	//look for range ?
	String *new_string = new String();
	*new_string = (*this)[pos];
	return ObjectRef(new_string);
}

//setIndex implementation for Vector<bool>
void Vector<bool>::setIndex(int pos, ObjectRef val) {
	//look for range ?
	RCPtr<Bool> v = val;
	(*this)[pos] = static_cast<basicType>(v->val());	
}

//setIndex implementation for Vector<int>
void Vector<int>::setIndex(int pos, ObjectRef val) {
	//look for range ?
	RCPtr<Int> v = val;
	(*this)[pos] = static_cast<basicType>(v->val());	
}

//setIndex implementation for Vector<float>
void Vector<float>::setIndex(int pos, ObjectRef val) {
	//look for range ?
	RCPtr<Float> v = val;
	(*this)[pos] = static_cast<basicType>(v->val());	
}

//setIndex implementation for Vector<double>
void Vector<double>::setIndex(int pos, ObjectRef val) {
	//look for range ?
	RCPtr<Double> v = val;
	(*this)[pos] = static_cast<basicType>(v->val());	
}
   
//setIndex implementation for Vector<complex<float> >
void Vector<complex<float> >::setIndex(int pos, ObjectRef val) {
	//look for range ?
	RCPtr<Complex<float> > v = val;
	(*this)[pos] = static_cast<basicType>(v->val());	
}

//setIndex implementation for Vector<complex<double> >
void Vector<complex<double> >::setIndex(int pos, ObjectRef val) {
	//look for range ?
	RCPtr<Complex<double> > v = val;
	(*this)[pos] = static_cast<basicType>(v->val());	
}

//setIndex implementation for Vector<string>
void Vector<string>::setIndex(int pos, ObjectRef val) {
	//look for range ?
	RCPtr<String> v = val;
	(*this)[pos] = static_cast<basicType>(v->val());	
}

//setIndex implementation for Vector<String>
void Vector<String>::setIndex(int pos, ObjectRef val) {
	//look for range ?
	RCPtr<String> v = val;
	(*this)[pos] = static_cast<basicType>(v->val());	
}

//setIndex implementation for Vector<ObjectRef>
void Vector<ObjectRef>::setIndex(int pos, ObjectRef val) {
        //look for range ?
        (*this)[pos] = val;
}

//pushBack implementation for Vector<bool>
void Vector<bool>::pushBack(ObjectRef val) {
	RCPtr<Bool> v = val;
	this->push_back(static_cast<basicType>(v->val()));
}

//pushBack implementation for Vector<int>
void Vector<int>::pushBack(ObjectRef val) {
	RCPtr<Int> v = val;
	this->push_back(static_cast<basicType>(v->val()));
}

//pushBack implementation for Vector<float>
void Vector<float>::pushBack(ObjectRef val) {
	RCPtr<Float> v = val;
	this->push_back(static_cast<basicType>(v->val()));
}

//pushBack implementation for Vector<double>
void Vector<double>::pushBack(ObjectRef val) {
	RCPtr<Double> v = val;
	this->push_back(static_cast<basicType>(v->val()));
}

//pushBack implementation for Vector<complex<float> >
void Vector<complex<float> >::pushBack(ObjectRef val) {
	RCPtr<Complex<float> > v = val;
	this->push_back(static_cast<basicType>(v->val()));
}

//pushBack implementation for Vector<complex<double> >
void Vector<complex<double> >::pushBack(ObjectRef val) {
	RCPtr<Complex<double> > v = val;
	this->push_back(static_cast<basicType>(v->val()));
}

//pushBack implementation for Vector<string>
void Vector<string>::pushBack(ObjectRef val) {
	RCPtr<String> v = val;
	this->push_back(static_cast<basicType>(v->val()));
}

//pushBack implementation for Vector<string>
void Vector<String>::pushBack(ObjectRef val) {
	RCPtr<String> v = val;
	this->push_back(static_cast<basicType>(v->val()));
}

//pushBack implementation for Vector<ObjectRef>
void Vector<ObjectRef>::pushBack(ObjectRef val) {
	this->push_back(val);
}

