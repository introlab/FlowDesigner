// Copyright (C) 1999 Jean-Marc Valin

#ifndef OBJECTPARSER_H
#define OBJECTPARSER_H

#include <iostream>
#include <vector>
#include "Object.h"
#include <map>

inline bool isValidType (istream &in, string expectedType, bool binary=false);

template <class T>
inline ostream &operator << (ostream &out, const RCPtr<T> &ref)
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

/*The following code doesn't compile with MSVC++*/
#ifndef BROKEN_TEMPLATES

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
#endif

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
	 if (in.fail()) 
	    throw new GeneralException("Error reading vector: '>' expected", __FILE__, __LINE__);
      }
      T tmp;
      in >> tmp;
      if (in.fail()) 
	 throw new GeneralException("Error reading vector", __FILE__, __LINE__);
      v.push_back(tmp);
   }
}

/*The following code doesn't compile with MSVC++*/
#ifndef BROKEN_TEMPLATES

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
	 if (in.fail()) 
	    throw new GeneralException("Error reading vector: '>' expected", __FILE__, __LINE__);
      }
      T *tmp = new T;
      in >> *tmp;
      if (in.fail()) 
	 throw new GeneralException("Error reading vector", __FILE__, __LINE__);
      v.push_back(tmp);
   }
}
#endif

class ParsingException : public BaseException{
public:
   ParsingException (string _message) 
      : message(_message) 
   {}
   void print(ostream &out=cerr)  {out << message << endl;}
protected:
   string message;
};


inline bool isValidType (istream &in, string expectedType, bool binary)
{
   char ch;
   in >> ch;
   if ((ch == '<' && !binary) || (ch == '{' && binary))
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




template <class T>
inline istream &operator >> (istream &in, RCPtr<T> &o)
{
   char ch;
   in >> ch;
   if (ch == '<')
   {
      string type;
      in >> type;
      o = Object::newObject(type);
      o->readFrom(in);
   } else if (ch == '{')
   {
      string type;
      in >> type;
      o = Object::newObject(type);
      int dummyCount=0;
      char dummy;
      do {
         in >> dummy;
	 if (dummyCount > 5)
	    throw new ParsingException("Cannot find sync \"|\" symbol for unserialize");
	 dummyCount++;
      } while(dummy != '|');
      o->unserialize(in);
   } else {
      in.putback(ch);
   }
   
   return in;  
}

template <class T>
inline istream &operator >> (istream &in, T* &o)
{
   RCPtr<T> obj;
   in >> obj;
   o = obj.detach();
   return in;
}

#endif
