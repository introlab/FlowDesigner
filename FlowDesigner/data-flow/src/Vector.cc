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
