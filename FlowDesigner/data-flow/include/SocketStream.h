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

/**

Base class for network sockets. The class supports two types of communication :
BROADCAST_TYPE and TCP_STREAM_TYPE. According to the type of communication, the user
will have to use socket_connect(...) for TCP_STREAM_TYPE (client), and socket_listen(...), 
socket_accept(...) for TCP_STREAM_TYPE (server). For the BROADCAST_TYPE, the user don't have
to use these functions. I/O is performed using send_packet(...) and recv_packet(...).

\author Dominic Letourneau
\date 03/10/2001

*/
class network_socket {

 public:

  ///Broadcast type on the subnet (mask = 255.255.255.0)
  static const int BROADCAST_TYPE;

  ///TCP stream standard communication type
  static const int TCP_STREAM_TYPE;

 public:

  /**
     Constructor with type and port params.

     \param type The type of socket : BROADCAST_TYPE or TCP_STREAM_TYPE
     \param port The network port
  */
  network_socket(int type, int port);

  
  ///Destructor, will terminate connexion calling shutdown()
  ~network_socket();

  /**
     Useful for IStream and OStream (deriving for Object) <br>
     <b>Format :</b> \<network_socket Type <i>m_type</i> Port <i>m_port</i> \>
  */
  void printOn (std::ostream &out) const;

  ///Init broadcast communication
  void init_broadcast();


  /**
     Send data on the network socket.
     \param packet Byte pointer to the data to send.
     \param size the number of bytes to send
  */
  size_t send_packet(unsigned char *packet, size_t size);


  /**
     Read data from the network socket.
     \param packet Byte pointer to the data to receive.
     \param packet Max size of the receiver buffer.
  */
  size_t recv_packet(unsigned char *packet, size_t size);

  ///Shutdown network socket (will end communication).
  void shutdown();

  /**
     Connect to a host name.
     \param host host name
  */
  void socket_connect(const char *host);


  /**
     Listen to a socket (server)
     \param backlog Number of "listen" to do.
     \param blocking Blocking on "listen" ?
  */
  void socket_listen(int backlog, bool blocking);


  /**
     Accept from a socket (server), listen must have been done before.          
  */
  void socket_accept();

  /**
     Returns the type of socket.
     \return int m_type
  */
  int get_type() {return m_type;}


  /**
     Return the port of the socket.
     \return int m_port
  */
  int get_port() {return m_port;}

  /** 
      Return the read socket
      \return int m_read_socket
  */
  int get_read_socket() {return m_read_socket;}

  /**
     Return the write socket
     \return int m_write_socket
  */
  int get_write_socket() {return m_write_socket;}

  /** 
      Return the listen socket
      \return int m_listen_socket
  */
  int get_listen_socket() {return m_listen_socket;}

 private:
  
  /// Initializes TCP stream connection
  void init_tcp_stream(bool blocking);

  /// Blocking or non blocking socket
  bool m_blocking;

  /// Port of socket
  int m_port;

  /// Type of socket
  int m_type;

  /// Listen socket
  int m_listen_socket;
  
  /// Write socket info
  int m_write_socket;
  sockaddr_in m_write_addr;
  
  /// Read socket info
  int m_read_socket;
  sockaddr_in m_read_addr;

};

/**
   streambuf wrapper for network sockets.
   
   \author Dominic Letourneau
   \date 03/10/2001
*/
class socket_streambuf : public std::streambuf, public network_socket {

  protected:
   virtual int overflow(int = EOF);
   virtual std::streamsize xsputn(const char *s, std::streamsize n);

   virtual int uflow();
   virtual int underflow();
   virtual std::streamsize xsgetn(char *s, std::streamsize n);
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

/**
   ostream wrapper for network sockets.
   
   \author Dominic Letourneau
   \date 03/10/2001
*/
class socket_ostream : public std::ostream {
  socket_streambuf _streambuffer; 
  public:

  operator socket_streambuf&(){return _streambuffer;}

  socket_ostream(int type, int port)
    : _streambuffer (type,port)
    , std::ostream(&_streambuffer)
    {clear();}
};

/**
   istream wrapper for network sockets.
   
   \author Dominic Letourneau
   \date 03/10/2001
*/
class socket_istream : public std::istream {
  socket_streambuf _streambuffer;
  public:

  operator socket_streambuf&(){return _streambuffer;}
  
  socket_istream(int type, int port)
    : _streambuffer (type, port)
    , std::istream(&_streambuffer)
    {clear();}
};

/**
   iostream wrapper for network sockets.
   
   \author Dominic Letourneau
   \date 03/10/2001
*/
class socket_iostream : public std::iostream {
  socket_streambuf _streambuffer;
  public:

  operator socket_streambuf&(){return _streambuffer;}
  
  socket_iostream(int type, int port)
    : _streambuffer (type, port)
    , std::iostream(&_streambuffer)
    {clear();}
};

#endif
