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

#ifndef OBJECTPARSER_H
#define OBJECTPARSER_H

#include <stream.h>
#include <iostream.h>
#include <vector>
#include <Object.h>
#include <map>

inline ostream &operator << (ostream &out, const ObjectRef &ref)
{
   out << *ref;
   return out;
}

template <class T>
inline ostream &operator << (ostream &out, const vector<T> &v)
{
   for (int i=0; i < v.size(); i++)
   {
      out << " " << v[i];
   }
   return out;
}

template <class T>
inline ostream &operator << (ostream &out, const vector<T*> &v)
{
   for (int i=0; i < v.size(); i++)
   {
      out << " " << *(v[i]);
   }
   return out;
}

template <class T>
inline istream &operator >> (istream &in, vector<T> &v)
{
   int items_found=0;

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

   return in;
}

template <class T>
inline istream &operator >> (istream &in, vector<T*> &v)
{
   int items_found=0;
   string nimportequoi;

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
   char ch;
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


class _ObjectFactory
{
public:
   virtual ObjectRef create() = 0;
};

template <class T>
class ObjectFactory : public _ObjectFactory {
public:
   virtual ObjectRef create() {return ObjectRef(new T);}
};

template <class T>
inline istream &operator >> (istream &in, Ptr<T> &o)
{
   char ch;
   in >> ch;
   if (ch != '<'){
      in.putback(ch);
      in.clear(ios::failbit);
      return in;
   }

   string type;
   in >> type;
   o = Object::newObject(type);
   o->readFrom(in);

   return in;  
}

#endif
