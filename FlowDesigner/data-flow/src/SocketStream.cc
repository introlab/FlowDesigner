#include "SocketStream.h"
#include "BaseException.h"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <fcntl.h>
#include <errno.h>

const int network_socket::BROADCAST_TYPE = 0;
const int network_socket::TCP_STREAM_TYPE = 1;

#define FLOWDESIGNER_IDENT_STRLEN 8
#define FLOWDESIGNER_IDENT "OVERFLOW"
#define FLOWDESIGNER_BROADCAST_IP "255.255.255.255"

network_socket::network_socket(int type, int port) 
: m_read_socket(0)
  , m_listen_socket(0)
, m_write_socket(0), m_port(port), m_type(type) {

  switch (m_type) {
  case BROADCAST_TYPE:
    init_broadcast();
    break;

  case TCP_STREAM_TYPE:
    break;

  default:
    throw new GeneralException("Unknown packet type",__FILE__,__LINE__);
    break;
  }


}

network_socket::~network_socket() {

  //shutting down communication
  shutdown();
}

void network_socket::printOn (ostream &out) const {

  out<<"network_socket"<<endl;
  out<<"Type : "<<m_type<<endl;
  out<<"Port : "<<m_port<<endl;

}

void network_socket::init_broadcast() {

  printf("Broadcast device initialising...");
    
  // Set up the write socket (datagram type)
  //
  m_write_socket = socket(AF_INET, SOCK_DGRAM, 0);

  if (m_write_socket == -1) {
    perror(__PRETTY_FUNCTION__);
    throw new GeneralException("Unable to create write_socket",__FILE__,__LINE__);
  }

  
  //initialising write structure
  memset(&m_write_addr, 0, sizeof(m_write_addr));
  
  m_write_addr.sin_family = AF_INET;
  m_write_addr.sin_addr.s_addr = inet_addr(FLOWDESIGNER_BROADCAST_IP);
  m_write_addr.sin_port = htons(m_port);
  
  // Set write socket options to allow broadcasting
  //
  u_int broadcast = 1;
  if (setsockopt(m_write_socket, SOL_SOCKET, SO_BROADCAST,(const char*)&broadcast, sizeof(broadcast)) < 0) {
    perror(__PRETTY_FUNCTION__);
    throw new GeneralException("Unable to allow broadcasting for write socket",__FILE__,__LINE__);
  }
    
  // Set up the read socket
  //
  m_read_socket = socket(PF_INET, SOCK_DGRAM, 0);
  if (m_read_socket == -1) {
    perror(__PRETTY_FUNCTION__);
    throw new GeneralException("Unable to create read_socket",__FILE__,__LINE__);
  }

  // Set socket options to allow sharing of port
  //
  u_int share = 1;
  if (setsockopt(m_read_socket, SOL_SOCKET, SO_REUSEADDR,(const char*)&share, sizeof(share)) < 0) {
    perror(__PRETTY_FUNCTION__);
    throw new GeneralException("Unable to share port for read_socket",__FILE__,__LINE__);
  }
    
  // Bind socket to port (any address)
  //
  memset(&m_read_addr, 0, sizeof(m_read_addr));
  m_read_addr.sin_family = AF_INET;
  m_read_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  m_read_addr.sin_port = htons(m_port);
  if (bind(m_read_socket, (sockaddr*) &m_read_addr, sizeof(m_read_addr)) < 0) {
    perror(__PRETTY_FUNCTION__);
    throw new GeneralException("Unable to bind read_socket",__FILE__,__LINE__);
  }

  // Set socket to non-blocking
  //
  if (fcntl(m_read_socket, F_SETFL, O_NONBLOCK) < 0) {
    perror(__PRETTY_FUNCTION__);
    throw new GeneralException("Unable set read_socket non-blocking",__FILE__,__LINE__);
  }

  //non blocking connexion
  m_blocking = false;

  printf("done\n");
}


void network_socket::shutdown() {

    printf("Shuting down...");
    
    // Close sockets
    //

    if (m_write_socket == m_read_socket) {
      close(m_write_socket);
    }
    else {
      close(m_write_socket);
      close(m_read_socket);
    }

    if (m_listen_socket) {
       close(m_listen_socket);
    }

    printf("done\n");
    
}

///////////////////////////////////////////////////////////////////////////
// Send a packet
//
size_t network_socket::send_packet(unsigned char *packet, size_t size) {
    
  unsigned int flags = 0;

  switch (m_type) {
  case BROADCAST_TYPE:
    
    if (sendto(m_write_socket, (const char*)packet, size,
	       0, (sockaddr*) &m_write_addr, sizeof(m_write_addr)) < 0) {
      perror(__PRETTY_FUNCTION__);
      throw new GeneralException("Unable to send packet",__FILE__,__LINE__);
      return 0;
    }
    
    break;

  case TCP_STREAM_TYPE:
    
    if (send(m_write_socket, (const char*)packet, size,flags) < 0) {
      perror(__PRETTY_FUNCTION__);
      throw new GeneralException("Unable to send packet",__FILE__,__LINE__);
      return 0;
    }
    break;

  default:
    throw new GeneralException("Unknown packet type",__FILE__,__LINE__);
    break;
  }

  return size;
}


///////////////////////////////////////////////////////////////////////////
// Receive a packet
//
size_t network_socket::recv_packet(unsigned char *packet, size_t size) {


  size_t packet_len = 0;
  unsigned int flags = 0;
  size_t addr_len = sizeof(m_read_addr); 

  switch (m_type) {
  case BROADCAST_TYPE:
         
    packet_len = recvfrom(m_read_socket, (char*)packet, size, 0, (sockaddr*) &m_read_addr, &addr_len);
    
    if ((int) packet_len < 0) {
      if (errno == EAGAIN) {
	return 0;
      }
      else {
	perror(__PRETTY_FUNCTION__);
	throw new GeneralException("Unable to recv packet",__FILE__,__LINE__);
	return 0;
      }
    }
    break;
    
  case TCP_STREAM_TYPE:
    

    
    packet_len = recv(m_read_socket,packet,size,flags);
    
    if ((int) packet_len < 0) {
      if (errno == EAGAIN) {
	return 0;
      }
      else {
	perror(__PRETTY_FUNCTION__);
	throw new GeneralException("Unable to recv packet",__FILE__,__LINE__);
	return 0;
      }
    }
    break;
      
  default:
    throw new GeneralException("Unknown packet type",__FILE__,__LINE__);
    break;
  }

  //printf("read packet len = %d \n", (int) packet_len);
  return packet_len;
}

void network_socket::init_tcp_stream(bool blocking) {

  struct sockaddr_in serverp;
  int address_size;// size of server address struct 
  struct hostent* entp;
  char host[256];
  int flags;// temp for old socket access flags 
  int one = 1;
  char* first_dot;

  
  m_blocking = blocking;

  
  address_size = sizeof(serverp);

  if(gethostname(host,256) == -1) {
    throw new GeneralException("network_socket::init_tcp_stream : couldn't get hostname.",__FILE__,__LINE__);
  }

  /* now, strip down to just the hostname */
  if((first_dot = strchr(host,'.'))) {
    *first_dot = '\0';
  }

  cerr<<"current host : "<<host<<endl;

  if((entp = gethostbyname(host)) == NULL) {
    cerr<<"Did not find host : "<<host<<endl;
    throw new GeneralException("network_socket::init_tcp_stream : host unknown.",__FILE__,__LINE__);
  }

  memcpy(&(serverp.sin_addr), entp->h_addr_list[0], entp->h_length);


  serverp.sin_port = htons(m_port);

  /* 
   * Create the INET socket.  
   * 
   */
  if((m_listen_socket = socket(PF_INET, SOCK_STREAM, 0)) == -1) {
    perror("network_socket::init_tcp_stream : call to socket() failed; socket not created.");
    throw new GeneralException("network_socket::init_tcp_stream : socket not created.",__FILE__,__LINE__);
  }

  /*
   * let's say that our process should get the SIGIO's when it's
   * readable
   */

  if(fcntl(m_listen_socket, F_SETOWN, getpid()) == -1) {

    perror("network_socket::init_tcp_stream : call to fcntl() failed while setting socket "
	   "pid ownership; socket not created.");
    shutdown();

    throw new GeneralException("network_socket::init_tcp_stream : could not change ownership of the socket."
			       ,__FILE__,__LINE__);    
  }

  /*
   * get the current access flags
   */
  if((flags = fcntl(m_listen_socket, F_GETFL)) == -1) {
    
    perror("network_socket::init_tcp_stream : call to fcntl failed while getting socket "
	   "access flags; socket not created.");
    shutdown();
    
    throw new GeneralException("network_socket::init_tcp_stream : could not get flags of the socket."
			       ,__FILE__,__LINE__);    
  }

  if(!blocking) {
      /*
       * OR the current flags with 
       * O_NONBLOCK (so we won't block), and write them back
       */
      
      if(fcntl(m_listen_socket, F_SETFL, flags | O_NONBLOCK ) == -1) {
	
	perror("network_socket::init_tcp_stream : call to :fcntl() failed while setting socket "
	       "access flags; socket not created.");
	shutdown();
	
	throw new GeneralException("network_socket::init_tcp_stream : could not set flags (O_NONBLOCK) of the socket."
				 ,__FILE__,__LINE__);   
	
      }
  }//non blocking
    

  /* make sure we can reuse the port soon after */
 
  if(setsockopt(m_listen_socket, SOL_SOCKET, SO_REUSEADDR, (const char*)&one, sizeof(one))) {
    
    perror("network_socket::init_tcp_stream : setsockopt(2) failed");
    
    throw new GeneralException("network_socket::init_tcp_stream : setsocktopt failed."
				 ,__FILE__,__LINE__);   
    
  }
  

  /* 
   * Bind it to the port indicated
   * INADDR_ANY indicates that any network interface (IP address)
   * for the local host may be used (presumably the OS will choose the 
   * right * one).
   *
   * Specifying sin_port = 0 would allow the system to choose the port.
   */
  serverp.sin_family = PF_INET;
  serverp.sin_addr.s_addr = INADDR_ANY;

  if(bind(m_listen_socket, (struct sockaddr*) &serverp, sizeof(serverp)) == -1) {

    perror ("network_socket::init_tcp_stream : bind() failed; socket not created.");
    shutdown();

    throw new GeneralException("network_socket::init_tcp_stream : bind failed."
				 ,__FILE__,__LINE__); 
  }


  cerr<<"init_tcp_stream done!"<<endl;
  
}

void network_socket::socket_listen(int backlog, bool blocking) {

  init_tcp_stream(blocking);

  cerr<<"listening"<<endl;
  
  if(listen(m_listen_socket,backlog)) {
    
    perror("network_socket::init_tcp_stream : listen(2) failed:");
    shutdown();
    
    throw new GeneralException("network_socket::init_tcp_stream : listen failed."
			       ,__FILE__,__LINE__); 
    
  }
}

void network_socket::socket_accept() {

  if (m_blocking) {
    cerr<<"accept (blocking)"<<endl;
  }
  else {
    cerr<<"accept (non-blocking)"<<endl;
  }

  //int accept(int s, struct sockaddr *addr, int *addrlen);
  socklen_t length;

  if((m_read_socket = accept(m_listen_socket,(struct sockaddr *)NULL, &length)) == -1) {
    perror("network_socket::server_accept error when calling accept()");
    shutdown();
    throw new GeneralException("network_socket::server_accept error when calling accept()",__FILE__,__LINE__);
  }
    
  //write & read sockets are the same
  m_write_socket = m_read_socket;

  //set the socket non blocking?
  
  //sending Overflow identification banner
  cerr<<"send banner"<<endl;

  unsigned char banner[] = {'O','V','E','R','F','L','O','W'};

  send_packet(&banner[0],FLOWDESIGNER_IDENT_STRLEN);

  cerr<<"accept done!"<<endl;
}

void network_socket::socket_connect(const char *host) {

  struct sockaddr_in server;
  struct hostent* entp;
  int sock;
  unsigned char banner[FLOWDESIGNER_IDENT_STRLEN];
  int numread;

  /* fill in server structure */
  server.sin_family = PF_INET;

  /* 
   * this is okay to do, because gethostbyname(3) does no lookup if the 
   * 'host' * arg is already an IP addr
   */
  if((entp = gethostbyname(host)) == NULL) {
    char message[256];
    sprintf(message, "player_connect() \"%s\" is an unknown host", host);
    throw new GeneralException(message,__FILE__,__LINE__);
  }

  memcpy(&server.sin_addr, entp->h_addr_list[0], entp->h_length);

  //setting port
  server.sin_port = htons(m_port);

  /* make our socket (and leave it blocking) */
  if((m_write_socket = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
    perror("network_socket::connect(): socket() failed");
    throw new GeneralException("network_socket::connect connect() failed",__FILE__,__LINE__);
  }

  //same socket for read & write
  m_read_socket = m_write_socket;

  /* 
   * hook it up
   */
  if(connect(m_write_socket, (struct sockaddr*)&server, sizeof(server)) == -1) {
    perror("network_socket::connect(): connect() failed");
    shutdown();
    throw new GeneralException("network_socket::connect(): connect() failed",__FILE__,__LINE__);
  }

// CC- Disable banner features. 01/12/2003
//  /*
//   * read the banner from the server
//   */
//  if((numread = recv_packet(&banner[0],FLOWDESIGNER_IDENT_STRLEN)) != FLOWDESIGNER_IDENT_STRLEN) {
//    perror("network_socket::connect(): read() failed");
//    shutdown();
//    throw new GeneralException("network_socket::connect(): read() failed",__FILE__,__LINE__);
//  }
//
//  /*
//   * verify the banner (if it contains "OVERFLOW")
//   */
//  
//  if (banner[0] != 'O' || banner[1] != 'V' || banner[2] != 'E' || banner[3] != 'R' ||
//      banner[4] != 'F' || banner[5] != 'L' || banner[6] != 'O' || banner[7] != 'W') {
//    shutdown(); 
//    throw new GeneralException("network_socket::connect() does not contain the banner",__FILE__,__LINE__); 
//  } 


}




/******************************************************************************
 Stream part!
*******************************************************************************/

socket_streambuf::socket_streambuf(int type, int port)
  : network_socket(type,port)
   , takeFromBuf(false) {

   
}

int socket_streambuf::overflow(int c)
{

   send_packet((unsigned char*) &c, 1);

   //FIXME: Find EOF?

   return 0;
}


streamsize socket_streambuf::xsputn(const char *s, streamsize n) {

  return send_packet((unsigned char*) s, (size_t) n);

}

int socket_streambuf::uflow() {

  if (takeFromBuf) {
    takeFromBuf = false;
    return charBuf;
  } else {
    recv_packet((unsigned char*) &charBuf, 1);
    return charBuf;      
  }
}

int socket_streambuf::underflow() {

   if (takeFromBuf) {
     return charBuf;
   } 
   else {
     recv_packet((unsigned char*) &charBuf, 1);
     takeFromBuf = true;
     return charBuf;
   }
}

int socket_streambuf::pbackfail(int c) {

  if (!takeFromBuf) {

    if (c != EOF)
      charBuf = c;
      
    takeFromBuf = true;
    return charBuf;
  } else {
    return EOF;
  }
}

streamsize socket_streambuf::xsgetn(char *s, streamsize n) {

  return recv_packet((unsigned char*) s, (size_t) n);

}
