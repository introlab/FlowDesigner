#ifndef OBJECTPARSER_H
#define OBJECTPARSER_H

#include <stream.h>
#include <iostream.h>
#include <vector>

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

#endif
