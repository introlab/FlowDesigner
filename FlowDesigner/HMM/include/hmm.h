// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#ifndef HMM_H
#define HMM_H

#include <vector>
#include <typeinfo>
#include <stream.h>
#include <string>

///Frame definition
typedef vector<float> Frame;

///Square function
template <class T>
inline T sqr(T x) {return x*x;}

template <class T>
inline ostream &operator << (ostream &out, const vector<T> &v)
{
   //out << "<VECTOR_" << typeid(T).name();
   for (int i=0; i < v.size(); i++)
      //for (vector<T>::const_iterator i=v.start(); i != v.end(); i++)
   {
      out << " " << v[i];
   }
   //out << ">\n";
   return out;
}

template <class T>
inline ostream &operator << (ostream &out, const vector<T*> &v)
{
   //out << "<VECTOR_" << typeid(T).name() << "Ptr";
   for (int i=0; i < v.size(); i++)
      //for (vector<T>::const_iterator i=v.start(); i != v.end(); i++)
   {
      out << " " << *(v[i]);
   }
   //out << ">\n";
   return out;
}

template <class T>
inline istream &operator >> (istream &in, vector<T> &v)
{
   cerr << "parsing vector of " << typeid(T).name() << "s" << endl;
   int items_found=0;
   //string nimportequoi;
   //in >> nimportequoi;
   while (!in.eof())
   {
      T tmp;
      in >> tmp;
      if (in.fail()) break;
      items_found++;
      v.resize(items_found);
      v[items_found-1]=tmp;
   }
   in.clear();
   //in >> nimportequoi;
   //cerr << "terminator: " << nimportequoi << endl;
   return in;
}

template <class T>
inline istream &operator >> (istream &in, vector<T*> &v)
{
   cerr << "parsing vector of " << typeid(T).name() << " pointers" << endl;
   int items_found=0;
   string nimportequoi;
   //in >> nimportequoi;
   //cerr << "vector type: " << nimportequoi << endl;
   while (!in.eof())
   {
      T *tmp = new T;
      in >> *tmp;
      if (in.fail()) break;
      items_found++;
      v.resize(items_found);
      v[items_found-1]=tmp;
   }
   in.clear();
   //in >> nimportequoi;
   //cerr << "terminator: " << nimportequoi << endl;
   return in;
}

class ParsingException {
public:
   ParsingException (string _message) 
      : message(_message) 
   {}
   void print(ostream &out=cerr) const {out << message << endl;}
protected:
   string message;
};


inline bool isValidType (istream &in, string expectedType)
{
   char ch;// = in.get();
   in >> ch;
   if (ch == '<')
   {
      string type;
      in >> type;
      if (type != expectedType)
         throw ParsingException ("Parser expected type " + expectedType + " and got " + type);
   } else {
      in.putback(ch);
      in.clear(ios::failbit);
      return false;
   }
   return true;
}


#endif
