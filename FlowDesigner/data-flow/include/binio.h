// Copyright (C) 2001 Jean-Marc Valin

#ifndef BINIO_H
#define BINIO_H

#include <iostream>

class BinIO {
public:
   static void _read(istream &in, void* data, size_t typeSize, size_t length);
   
   template<class T>
   static inline void read(istream &in, T* data, int length)
   {
      _read(in, (void *)data, sizeof(T), length);
   }

   static void _write(ostream &out, const void* data, size_t typeSize, size_t length);

   template<class T>
   static inline void write(ostream &out, const T* data, int length)
   {
      _write(out, (void *)data, sizeof(T), length);
   }

};

#endif
