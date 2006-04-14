// Copyright (C) 2001 Jean-Marc Valin

#ifndef BINIO_H
#define BINIO_H

#include <iostream>
#include <stdlib.h>

namespace FD {

class BinIO {
public:
   static void _read(std::istream &in, void* data, size_t typeSize, size_t length);
   
   template<class T>
   static inline void read(std::istream &in, T* data, int length)
   {
      _read(in, (void *)data, sizeof(T), length);
   }

   static void _write(std::ostream &out, const void* data, size_t typeSize, size_t length);

   template<class T>
   static inline void write(std::ostream &out, const T* data, int length)
   {
      _write(out, (void *)data, sizeof(T), length);
   }

};

}
#endif
