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

#include <iostream>
#include <vector>
#include "Object.h"
#include <map>

inline bool isValidType (istream &in, string expectedType);

inline ostream &operator << (ostream &out, const ObjectRef &ref)
{
   out << *ref;
   return out;
}

template <class T>
inline ostream &operator << (ostream &out, const vector<T> &v)
{
   out << "<Vector ";
   for (int i=0; i < v.size(); i++)
   {
      out << " " << v[i];
   }
   out << " > ";
   return out;
}

template <class T>
inline ostream &operator << (ostream &out, const vector<T*> &v)
{
   out << "<Vector ";
   for (int i=0; i < v.size(); i++)
   {
      out << " " << *(v[i]);
   }
   out << " > ";
   return out;
}

template <class T>
inline istream &operator >> (istream &in, vector<T> &v)
{
   int items_found=0;

   if (!isValidType(in,"Vector"))
      return in;

   while (1)
   {
      char ch=' ';
      while (ch == ' ')
      {
	 in >> ch;
	 if (ch == '>')
	 {
	    return in;
	 } else if (ch != ' ') {
	    in.putback(ch);
	 }
      }
      T tmp;
      in >> tmp;
      if (in.fail()) 
	 throw GeneralException("Error reading vector", __FILE__, __LINE__);
      v.push_back(tmp);
   }
}

template <class T>
inline istream &operator >> (istream &in, vector<T*> &v)
{
   int items_found=0;

   if (!isValidType(in,"Vector"))
      return in;

   while (1)
   {
      char ch=' ';
      while (ch == ' ')
      {
	 in >> ch;
	 if (ch == '>')
	 {
	    return in;
	 } else if (ch != ' ') {
	    in.putback(ch);
	 }
      }
      T *tmp = new T;
      in >> *tmp;
      if (in.fail()) 
	 throw GeneralException("Error reading vector", __FILE__, __LINE__);
      v.push_back(tmp);
   }
}

class ParsingException : public BaseException{
public:
   ParsingException (string _message) 
      : message(_message) 
   {}
   void print(ostream &out=cerr)  {out << message << endl;}
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
         throw new ParsingException ("Parser expected type " + expectedType + " and got " + type);
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
inline istream &operator >> (istream &in, RCPtr<T> &o)
{
   char ch;
   in >> ch;
   if (ch != '<'){
      in.putback(ch);
      //in.clear(ios::failbit);
      return in;
   }

   string type;
   in >> type;
   o = Object::newObject(type);
   o->readFrom(in);

   return in;  
}

#endif
