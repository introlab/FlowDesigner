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
#include "Complex.h"

//@implements core

namespace FD {

using namespace std;

DECLARE_TYPE(Char)
DECLARE_TYPE(Int)
DECLARE_TYPE(Bool)
DECLARE_TYPE(Float)
DECLARE_TYPE(Double)
DECLARE_TYPE(String)
DECLARE_TYPE(NilObject)
DECLARE_TYPE(Complex<float>)
DECLARE_TYPE(Complex<double>)
DECLARE_TYPE(NetCType<complex<float> >)
DECLARE_TYPE(NetCType<complex<double> >)

template<> vector<Char *> ObjectPool<Char >::stack = std::vector<Char *>();
template<> vector<Int *> ObjectPool<Int >::stack = std::vector<Int *>();
template<> vector<Bool *> ObjectPool<Bool >::stack = std::vector<Bool *>();
template<> vector<Float *> ObjectPool<Float >::stack = std::vector<Float *>();
template<> vector<Double *> ObjectPool<Double >::stack = std::vector<Double *>();
template<> vector<Complex<float> *> ObjectPool<Complex<float> >::stack = std::vector<Complex<float> *>();
template<> vector<Complex<double> *> ObjectPool<Complex<double> >::stack = std::vector<Complex<double> *>();
template<> vector<NetCType<complex<float> > *> ObjectPool<NetCType<complex<float> > >::stack = std::vector<NetCType<complex<float> > * >();
template<> vector<NetCType<complex<double> > *> ObjectPool<NetCType<complex<double> > >::stack = std::vector<NetCType<complex<double> > * >();

template<> FastMutex ObjectPool<Char >::mutex = FastMutex();
template<> FastMutex ObjectPool<Int >::mutex = FastMutex();
template<> FastMutex ObjectPool<Bool >::mutex = FastMutex();
template<> FastMutex ObjectPool<Float >::mutex = FastMutex();
template<> FastMutex ObjectPool<Double >::mutex = FastMutex();
template<> FastMutex ObjectPool<Complex<float> >::mutex = FastMutex();
template<> FastMutex ObjectPool<Complex<double> >::mutex = FastMutex();
template<> FastMutex ObjectPool<NetCType<complex<float> > >::mutex = FastMutex();
template<> FastMutex ObjectPool<NetCType<complex<double> > >::mutex = FastMutex();


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
   out << "{" << className() << endl;
   out << "|";
   int my_size = this->size();
   BinIO::write(out,&my_size, 1);      
   BinIO::write(out,this->c_str(),my_size);   
   out.put('}');
}
void String::unserialize(istream &in)
{   
	int my_size;
	BinIO::read(in, &my_size, 1);	
	this->resize(my_size);
	//FIXME reading one character at at time to avoid network problems
	//This is slow...
	for (int i = 0; i < my_size; i++) {
	    char *data_ptr = const_cast<char*>(this->c_str());
		BinIO::read(in, &data_ptr[i],1);
	}
	//reading last "}"
	char ch;
	in >> ch;	
}

void String::prettyPrint(ostream &out) const
{
   // CC : gcc 4.0 doesn't like this cast (caused weird outputs)
   // out << *(string*) (this);
   // Changed it for :
   out << (string)(*(this));
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

}//namespace FD

