// Copyright (C) 2001 Jean-Marc Valin

#include "binio.h"

void BinIO::_read(istream &in, void* data, size_t typeSize, size_t length)
{
#ifdef WORDS_BIGENDIAN
   in.read(data, typeSize*length);
#else
   char *orig = (char *)(data);
   char copy[length*typeSize];
   in.read(copy, typeSize*length);
   for (int i=0;i<length;i++)
      for (int j=0;j<typeSize;j++)
         orig[typeSize*(i+1)-1-j] = copy[typeSize*i+j];
#endif
}

void BinIO::_write(ostream &out, void* data, size_t typeSize, size_t length)
{
#ifdef WORDS_BIGENDIAN
   out.write(data, typeSize*length);
#else
   char *orig = (char *)(data);
   char copy[length*typeSize];
   for (int i=0;i<length;i++)
      for (int j=0;j<typeSize;j++)
         copy[typeSize*i+j] = orig[typeSize*(i+1)-1-j];
   out.write(copy, typeSize*length);
#endif
}

