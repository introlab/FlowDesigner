// Copyright (C) 2001 Jean-Marc Valin

#ifndef STREAM_WRAP_H
#define STREAM_WRAP_H

//#include <streambuf>
#include <iostream>
#include <stdio.h>

using namespace std;

class fileptr_streambuf : public streambuf {
  public:
   //typedef char_traits<char> _Tr;
  protected:
   virtual int uflow();
   virtual int underflow();
   virtual int overflow(int = EOF);
  public:
   fileptr_streambuf(FILE *_file, bool _owner=false);
   ~fileptr_streambuf() {if (owner) fclose (file);}
  protected:
   bool owner;
   FILE *file;
};

class fileptr_ostream : public ostream {
   fileptr_streambuf _streambuffer;
  public:
   fileptr_ostream(FILE *_file)
      : _streambuffer (_file)
      , ostream(&_streambuffer)
      {clear();}
};


class fileptr_istream : public istream {
   fileptr_streambuf _streambuffer;
  public:
   fileptr_istream(FILE *_file)
      : _streambuffer (_file)
      , istream(&_streambuffer)
      {clear();}
};

#endif
