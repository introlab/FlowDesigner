// Copyright (C) 2001 Jean-Marc Valin

#include "binio.h"

void BinIO::_sread(istream &in, void* data, size_t typeSize, size_t length)
{
#ifdef BIG_ENDIAN
#elif defined (LITTLE_ENDIAN)
#else
//#error unknown endianness
#endif
}

void BinIO::_swrite(ostream &out, void* data, size_t typeSize, size_t length)
{
}

