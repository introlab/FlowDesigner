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

//@implements core

//static int dummy = Object::addObjectType<Vector<float> > ("Vector", new ObjectFactory<Vector<float> > ("Vector"));
DECLARE_TYPE2("Vector", Vector<float>)
DECLARE_TYPE(Vector<float>)
DECLARE_TYPE(Vector<double>)
DECLARE_TYPE(Vector<int>)
DECLARE_TYPE(Vector<bool>)
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

void Vector<bool>::prettyPrint(ostream &out) const {
  for (unsigned int i = 0; i < size(); i++) {
    out<<(*this)[i]<<" ";
  }
  out<<endl;
}

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

ObjectRef addVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_add_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(addVtable, addVectorFloat, Vector<float>, Vector<float>);


ObjectRef mulVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_mul_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulVectorFloat, Vector<float>, Vector<float>);


ObjectRef subVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_sub_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(subVtable, subVectorFloat, Vector<float>, Vector<float>);


ObjectRef divVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_div_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(divVtable, divVectorFloat, Vector<float>, Vector<float>);



ObjectRef addVectorFloatFloat (ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   float v2 = dereference_cast<float> (y);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_add_scal(v2, &v1[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(addVtable, addVectorFloatFloat, Vector<float>, Float);

ObjectRef subVectorFloatFloat (ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   float v2 = dereference_cast<float> (y);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_add_scal(-v2, &v1[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(subVtable, subVectorFloatFloat, Vector<float>, Float);

ObjectRef mulVectorFloatFloat (ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   float v2 = dereference_cast<float> (y);
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   vec_mul_scal(v2, &v1[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulVectorFloatFloat, Vector<float>, Float);



ObjectRef addVectorComplexFloat(ObjectRef x, ObjectRef y)
{
   Vector<complex<float> > &v1 = object_cast<Vector<complex<float> > > (x);
   Vector<complex<float> > &v2 = object_cast<Vector<complex<float> > > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<complex<float> > *v = Vector<complex<float> >::alloc(length);
   vec_add_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(addVtable, addVectorComplexFloat, Vector<complex<float> >, Vector<complex<float> >);


ObjectRef mulVectorComplexFloat(ObjectRef x, ObjectRef y)
{
   Vector<complex<float> > &v1 = object_cast<Vector<complex<float> > > (x);
   Vector<complex<float> > &v2 = object_cast<Vector<complex<float> > > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<complex<float> > *v = Vector<complex<float> >::alloc(length);
   vec_mul_vec(&v1[0], &v2[0], &(*v)[0], length);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(mulVtable, mulVectorComplexFloat, Vector<complex<float> >, Vector<complex<float> >);


ObjectRef maxVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   for (int i=0;i<length;i++)
      (*v)[i] = max(v1[i], v2[i]);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(maxVtable, maxVectorFloat, Vector<float>, Vector<float>);


ObjectRef minVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);
   if (v1.size() != v2.size())
   {
      cerr << v1.size() << " != " << v2.size() << endl;
      throw new GeneralException("Vector size mismatch", __FILE__, __LINE__);
   }
   int length = v1.size();

   Vector<float> *v = Vector<float>::alloc(length);
   for (int i=0;i<length;i++)
      (*v)[i] = min(v1[i], v2[i]);
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(minVtable, minVectorFloat, Vector<float>, Vector<float>);



ObjectRef concatVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   Vector<float> &v2 = object_cast<Vector<float> > (y);

   Vector<float> *v = Vector<float>::alloc(v1.size()+v2.size());
   vec_copy(&v1[0], &(*v)[0], v1.size());
   vec_copy(&v2[0], &(*v)[v1.size()], v2.size());
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatVectorFloat, Vector<float>, Vector<float>);


ObjectRef concatVectorFloatFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (x);
   float f = dereference_cast<float> (y);

   Vector<float> *v = Vector<float>::alloc(v1.size()+1);
   vec_copy(&v1[0], &(*v)[0], v1.size());
   (*v)[v1.size()] = f;
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatVectorFloatFloat, Vector<float>, Float);

ObjectRef concatFloatVectorFloat(ObjectRef x, ObjectRef y)
{
   Vector<float> &v1 = object_cast<Vector<float> > (y);
   float f = dereference_cast<float> (x);

   Vector<float> *v = Vector<float>::alloc(v1.size()+1);
   vec_copy(&v1[0], &(*v)[1], v1.size());
   (*v)[0] = f;
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatFloatVectorFloat, Float, Vector<float>);

ObjectRef concatFloatFloat(ObjectRef x, ObjectRef y)
{
   float f1 = dereference_cast<float> (x);
   float f2 = dereference_cast<float> (y);

   Vector<float> *v = Vector<float>::alloc(2);
   (*v)[0] = f1;
   (*v)[1] = f2;
   return ObjectRef(v);
}
REGISTER_DOUBLE_VTABLE(concatVtable, concatFloatFloat, Float, Float);
