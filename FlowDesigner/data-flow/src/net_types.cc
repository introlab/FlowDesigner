// Copyright (C) 2001 Jean-Marc Valin

#include "net_types.h"
#include <stdio.h>

#ifndef WIN32
#include <unistd.h>
#endif /*ifdef WIN32*/

#include <iostream>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

//@implements core

DECLARE_TYPE(Int)
DECLARE_TYPE(Bool)
DECLARE_TYPE(Float)
DECLARE_TYPE(Double)
DECLARE_TYPE(String)

vector<Int *> ObjectPool<Int>::stack;
vector<Bool *> ObjectPool<Bool>::stack;
vector<Float *> ObjectPool<Float>::stack;
vector<Double *> ObjectPool<Double>::stack;

ObjectRef TrueObject(new Bool(true));
ObjectRef FalseObject(new Bool(false));


static void writeString(ostream &out, const String &str)
{
   for (unsigned int i=0;i<str.size();i++)
   {
      if (str[i] == '>')
      {
	 out.put('\\');
	 out.put('>');
      } else if (str[i] == '\\')
      {
	 out.put('\\');
	 out.put('\\');
      } else
	 out.put(str[i]);
   }
}

void String::printOn(ostream &out) const
{
   out << "<String ";
   writeString(out, *this);
   out.put('>');
   //out << "<String " << *(string*) (this) << " >";
}
void String::readFrom(istream &in)
{
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
        (*this) += ch;
     }
     else if (ch == ' ')
     {
	if (i)
	   (*this) += ch;
	else
	   continue;
	   }
     else if (ch == '>')
     {
        break;
     }
     else if (ch == '}')
     {
        break;
     }
     else
     {
        (*this) += ch;
     }
     i++;
   }
}
void String::serialize(ostream &out) const
{
   out << "{String |";
   writeString(out, *this);
   out.put('}');
   //out << "{String |" << *(string*) (this) << " }";
}
void String::unserialize(istream &in)
{
   readFrom(in);
}

void String::prettyPrint(ostream &out) const
{
   out << *(string*) (this);
}

istream &operator >> (istream &in, String &str)
{
   if (!isValidType(in, "String")) return in;
   str.readFrom(in);
   return in;
}


FILEPTR::FILEPTR(FILE *file) 
   : GenericType<FILE *> (file)
{}

FILEPTR::FILEPTR(const string &filename, const string &mode) 
   : GenericType<FILE *> (fopen(filename.c_str(), mode.c_str()))
{}

FILEPTR::~FILEPTR() 
{
   fclose(value);
}

#ifndef WIN32

FILEDES::FILEDES(int fd) 
   : GenericType<int> (fd)
{}

FILEDES::FILEDES(const string &filename, int mode) 
   : GenericType<int> (open(filename.c_str(), mode))
{}

FILEDES::~FILEDES() 
{
   close(value);
}


#endif /*ifdef WIN32*/
