// Copyright (C) 2001 Jean-Marc Valin

#ifndef STREAM_WRAP_H
#define STREAM_WRAP_H

//#include <streambuf>
#include <iostream>
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef WIN32
#include <unistd.h>
#endif
#include <string>

#include "Object.h"

using namespace std;


class EOFObject : public Object {
  public:
   void printOn(ostream &out = cout) const {out << "<EOFObject >\n";}
};


class fileptr_streambuf : public streambuf {
  protected:
   virtual int overflow(int = EOF);
   virtual streamsize xsputn(const char *s, streamsize n);

   virtual int uflow();
   virtual int underflow();
   virtual streamsize xsgetn(char *s, streamsize n);
   virtual int pbackfail(int c);
  public:
   fileptr_streambuf(FILE *_file, bool _owner=true, bool _isPipe=false);
   ~fileptr_streambuf() {if (owner) { if (isPipe) pclose(file); else fclose (file);}}
  protected:
   FILE *file;
   bool owner;
   bool takeFromBuf;
   char charBuf;
   bool isPipe;
};



class fileptr_ostream : public ostream {
   fileptr_streambuf _streambuffer;
  public:
   fileptr_ostream(FILE *_file, bool _owner=true, bool _isPipe=false)
      : _streambuffer (_file, _owner, _isPipe)
      , ostream(&_streambuffer)
      {clear();}
};

class fileptr_istream : public istream {
   fileptr_streambuf _streambuffer;
  public:
   fileptr_istream(FILE *_file, bool _owner=true, bool _isPipe=false)
      : _streambuffer (_file, _owner, _isPipe)
      , istream(&_streambuffer)
      {clear();}
};

class fileptr_iostream : public iostream {
   fileptr_streambuf _streambuffer;
  public:
   fileptr_iostream(FILE *_file, bool _owner=true, bool _isPipe=false)
      : _streambuffer (_file, _owner, _isPipe)
      , iostream(&_streambuffer)
      {clear();}
};




#ifndef WIN32

class fd_streambuf : public streambuf {
  protected:
   virtual int overflow(int = EOF);
   virtual streamsize xsputn(const char *s, streamsize n);

   virtual int uflow();
   virtual int underflow();
   virtual streamsize xsgetn(char *s, streamsize n);
   virtual int pbackfail(int c);
  public:
   fd_streambuf(int _fd, bool _owner=true);
   ~fd_streambuf() {if (owner) close (fd);}
  protected:
   int fd;
   bool owner;
   bool takeFromBuf;
   char charBuf;
};



class fd_ostream : public ostream {
   fd_streambuf _streambuffer;
  public:
   fd_ostream(int _fd, bool _owner=true)
      : _streambuffer (_fd, _owner)
      , ostream(&_streambuffer)
      {clear();}
};


class fd_istream : public istream {
   fd_streambuf _streambuffer;
  public:
   fd_istream(int _fd, bool _owner=true)
      : _streambuffer (_fd, _owner)
      , istream(&_streambuffer)
      {clear();}
};

class fd_iostream : public iostream {
   fd_streambuf _streambuffer;
  public:
   fd_iostream(int _fd, bool _owner=true)
      : _streambuffer (_fd, _owner)
      , iostream(&_streambuffer)
      {clear();}
};






class pipe_streambuf : public streambuf {
  protected:
   virtual int overflow(int = EOF);
   virtual streamsize xsputn(const char *s, streamsize n);

   virtual int uflow();
   virtual int underflow();
   virtual streamsize xsgetn(char *s, streamsize n);
   virtual int pbackfail(int c);
   int ll_read(void *buf, size_t count);
   int ll_write(const void *buf, size_t count);
  public:
   //pipe_streambuf(int _ifd, int _ofd, pid_t _pid, bool _waitOnClose=false);
   pipe_streambuf(const string &command, bool _waitOnClose=false);
   ~pipe_streambuf();
  protected:
   int ifd;
   int ofd;
   pid_t pid;
   bool waitOnClose;
   bool takeFromBuf;
   char charBuf;
};



class pipe_istream : public istream {
   pipe_streambuf _streambuffer;
  public:
   pipe_istream(const string &command, bool waitOnClose=false)
      : _streambuffer (command, waitOnClose)
      , istream(&_streambuffer)
      {clear();}
};

class pipe_iostream : public iostream {
   pipe_streambuf _streambuffer;
  public:
   pipe_iostream(const string &command, bool waitOnClose=false)
      : _streambuffer (command, waitOnClose)
      , iostream(&_streambuffer)
      {clear();}
};

#endif


#endif
