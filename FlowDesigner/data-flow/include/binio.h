// Copyright (C) 2001 Jean-Marc Valin

#ifndef BINIO_H
#define BINIO_H

#include <iostream>

class BinIO {
public:
   void _sread(istream &in, void* data, size_t typeSize, size_t length);
   
   template<class T>
   inline void sbinread(istream &in, T* data, int length)
   {
      _sread(in, (void *)data, sizeof(T), length);
   }

   void _swrite(ostream &out, void* data, size_t typeSize, size_t length);

   template<class T>
   inline void swrite(ostream &out, T* data, int length)
   {
      _swrite(out, (void *)data, sizeof(T), length);
   }

};

#endif
