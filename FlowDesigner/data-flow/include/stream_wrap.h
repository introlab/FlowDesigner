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


namespace FD {

class EOFObject : public Object {
  public:
   void printOn(std::ostream &out = std::cout) const {out << "<EOFObject >\n";}
};


class fileptr_streambuf : public std::streambuf {
  protected:
   virtual int overflow(int = EOF);
   virtual std::streamsize xsputn(const char *s, std::streamsize n);

   virtual int uflow();
   virtual int underflow();
   virtual std::streamsize xsgetn(char *s, std::streamsize n);
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



class fileptr_ostream : public std::ostream {
   fileptr_streambuf _streambuffer;
  public:
   fileptr_ostream(FILE *_file, bool _owner=true, bool _isPipe=false)
      : std::ostream(&_streambuffer)
      , _streambuffer (_file, _owner, _isPipe)
      {clear();}
};

class fileptr_istream : public std::istream {
   fileptr_streambuf _streambuffer;
  public:
   fileptr_istream(FILE *_file, bool _owner=true, bool _isPipe=false)
      : std::istream(&_streambuffer)
      , _streambuffer (_file, _owner, _isPipe)
      {clear();}
};

class fileptr_iostream : public std::iostream {
   fileptr_streambuf _streambuffer;
  public:
   fileptr_iostream(FILE *_file, bool _owner=true, bool _isPipe=false)
      : std::iostream(&_streambuffer)
      , _streambuffer (_file, _owner, _isPipe)
      {clear();}
};




#ifndef WIN32

class fd_streambuf : public std::streambuf {
  protected:
   virtual int overflow(int = EOF);
   virtual std::streamsize xsputn(const char *s, std::streamsize n);

   virtual int uflow();
   virtual int underflow();
   virtual std::streamsize xsgetn(char *s, std::streamsize n);
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



class fd_ostream : public std::ostream {
   fd_streambuf _streambuffer;
  public:
   fd_ostream(int _fd, bool _owner=true)
      : std::ostream(&_streambuffer)
      , _streambuffer (_fd, _owner)
      {clear();}
};


class fd_istream : public std::istream {
   fd_streambuf _streambuffer;
  public:
   fd_istream(int _fd, bool _owner=true)
      : std::istream(&_streambuffer)
      , _streambuffer (_fd, _owner)
      {clear();}
};

class fd_iostream : public std::iostream {
   fd_streambuf _streambuffer;
  public:
   fd_iostream(int _fd, bool _owner=true)
      : std::iostream(&_streambuffer)
      , _streambuffer (_fd, _owner)
      {clear();}
};






class pipe_streambuf : public std::streambuf {
  protected:
   virtual int overflow(int = EOF);
   virtual std::streamsize xsputn(const char *s, std::streamsize n);

   virtual int uflow();
   virtual int underflow();
   virtual std::streamsize xsgetn(char *s, std::streamsize n);
   virtual int pbackfail(int c);
   int ll_read(void *buf, size_t count);
   int ll_write(const void *buf, size_t count);
  public:
   //pipe_streambuf(int _ifd, int _ofd, pid_t _pid, bool _waitOnClose=false);
   pipe_streambuf(const std::string &command, bool _waitOnClose=false);
   ~pipe_streambuf();
   void pipeString(const std::string &in, std::string &out);
  protected:
   int ifd;
   int ofd;
   pid_t pid;
   bool waitOnClose;
   bool takeFromBuf;
   char charBuf;
};



class pipe_istream : public std::istream {
   pipe_streambuf _streambuffer;
  public:
   pipe_istream(const std::string &command, bool waitOnClose=false)
      : std::istream(&_streambuffer)
      , _streambuffer (command, waitOnClose)
      {clear();}
};

class pipe_iostream : public std::iostream {
   pipe_streambuf _streambuffer;
  public:
   pipe_iostream(const std::string &command, bool waitOnClose=false)
      : std::iostream(&_streambuffer)
      , _streambuffer (command, waitOnClose)
      {clear();}
   void pipeString(const std::string &in, std::string &out) {_streambuffer.pipeString(in,out);}
};

#endif

}//namespace FD
#endif
