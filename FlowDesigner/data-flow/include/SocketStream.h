#ifndef _SOCKETSTREAM_H_
#define _SOCKETSTREAM_H_

//Stream wrapper
//Dominic Letourneau 03/10/2001

#include <stddef.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "Object.h"
#include "net_types.h"
#include <iostream>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef WIN32
#include <unistd.h>
#endif

class network_socket {

 public:

  static const int BROADCAST_TYPE;
  static const int TCP_STREAM_TYPE;


 public:

  network_socket(int type, int port);

  ~network_socket();

  void printOn (ostream &out) const;

  void init_broadcast();

  size_t send_packet(unsigned char *packet, size_t size);

  size_t recv_packet(unsigned char *packet, size_t size);

  void shutdown();

  void socket_connect(const char *host);

  void socket_listen(int backlog, bool blocking);

  void socket_accept();

  int get_type() {return m_type;}

  int get_port() {return m_port;}

 private:
  
  void init_tcp_stream(bool blocking);

  bool m_blocking;

  int m_port;

  int m_type;

  // Listen socket
  //
  int m_listen_socket;
  
  // Write socket info
  //
  int m_write_socket;
  sockaddr_in m_write_addr;
  
  // Read socket info
  //
  int m_read_socket;
  sockaddr_in m_read_addr;

};

class socket_streambuf : public streambuf, public network_socket {

  protected:
   virtual int overflow(int = EOF);
   virtual streamsize xsputn(const char *s, streamsize n);

   virtual int uflow();
   virtual int underflow();
   virtual streamsize xsgetn(char *s, streamsize n);
   virtual int pbackfail(int c);

  public:
   socket_streambuf(int type, int port);

   virtual ~socket_streambuf() {

   }

  protected:

   bool owner;
   bool takeFromBuf;
   char charBuf;
};

class socket_ostream : public ostream {
  socket_streambuf _streambuffer; 
  public:

  operator socket_streambuf&(){return _streambuffer;}

  socket_ostream(int type, int port)
    : _streambuffer (type,port)
    , ostream(&_streambuffer)
    {clear();}
};


class socket_istream : public istream {
  socket_streambuf _streambuffer;
  public:

  operator socket_streambuf&(){return _streambuffer;}
  
  socket_istream(int type, int port)
    : _streambuffer (type, port)
    , istream(&_streambuffer)
    {clear();}
};

class socket_iostream : public iostream {
  socket_streambuf _streambuffer;
  public:

  operator socket_streambuf&(){return _streambuffer;}
  
  socket_iostream(int type, int port)
    : _streambuffer (type, port)
    , iostream(&_streambuffer)
    {clear();}
};

#endif
