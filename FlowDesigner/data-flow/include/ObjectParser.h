// Copyright (C) 1999 Jean-Marc Valin

#ifndef OBJECTPARSER_H
#define OBJECTPARSER_H

#include <iostream>
#include <vector>
#include "Object.h"
#include <map>
#include <string>

inline bool isValidType (std::istream &in, std::string expectedType, bool binary=false);

template <class T>
inline std::ostream &operator << (std::ostream &out, const RCPtr<T> &ref)
{
   out << *ref;
   return out;
}

template <class T>
inline std::ostream &operator << (std::ostream &out, const std::vector<T> &v)
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
inline std::ostream &operator << (std::ostream &out, const std::vector<T*> &v)
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
inline std::istream &operator >> (std::istream &in, std::vector<T> &v)
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

template <class T>
inline std::istream &operator >> (std::istream &in, std::vector<T*> &v)
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


class ParsingException : public BaseException{
public:
   ParsingException (std::string _message) 
      : message(_message) 
   {}
   void print(std::ostream &out=std::cerr)  {out << message << std::endl;}
protected:
   std::string message;
};


inline bool isValidType (std::istream &in, std::string expectedType, bool binary)
{
   char ch;
   in >> ch;
   if ((ch == '<' && !binary) || (ch == '{' && binary))
   {
      std::string type;
      in >> type;
      if (type != expectedType)
         throw new ParsingException ("ObjectParser::isValidType : Parser expected type " + expectedType + " and got " + type);
   } else {
      in.putback(ch);
      in.clear(std::ios::failbit);
      return false;
   }
   return true;
}




template <class T>
inline std::istream &operator >> (std::istream &in, RCPtr<T> &o)
{
   char ch;
   in >> ch;
   if (ch == '<')
   {
      std::string type;
      in >> type;
      o = Object::newObject(type);
      o->readFrom(in);
   } else if (ch == '{')
   {
      std::string type;
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
      throw new ParsingException(std::string("Expected '<' or '{' (got '") + ch + "')");
   }
   
   return in;  
}

template <class T>
inline std::istream &operator >> (std::istream &in, T* &o)
{
   RCPtr<T> obj;
   in >> obj;
   o = obj.detach();
   return in;
}

#endif
