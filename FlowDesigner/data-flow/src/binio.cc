// Copyright (C) 2001 Jean-Marc Valin

#include "binio.h"
#include "misc.h"

using namespace std;
using namespace FD;

//@implements core

void BinIO::_read(istream &in, void* data, size_t typeSize, size_t length)
{
#ifdef WORDS_BIGENDIAN
   in.read(data, typeSize*length);
#else
   char *orig = (char *)(data);
   //char copy[length*typeSize];
   DYN_VEC(char, length*typeSize, copy);
   in.read(copy, typeSize*length);
   for (unsigned int i=0;i<length;i++)
      for (unsigned int j=0;j<typeSize;j++)
         orig[typeSize*(i+1)-1-j] = copy[typeSize*i+j];
#endif
}

void BinIO::_write(ostream &out, const void* data, size_t typeSize, size_t length)
{
#ifdef WORDS_BIGENDIAN
   out.write(data, typeSize*length);
#else
   char *orig = (char *)(data);
   //char copy[length*typeSize];
   DYN_VEC(char, length*typeSize, copy);
   for (unsigned int i=0;i<length;i++)
      for (unsigned int j=0;j<typeSize;j++)
         copy[typeSize*i+j] = orig[typeSize*(i+1)-1-j];
   out.write(copy, typeSize*length);
#endif
}

