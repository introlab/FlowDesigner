
// Copyright (C) 2004 Dominic Letourneau

#include <string.h>

#include "URLHandler.h"
#include <string>
#include <map>
#include "Object.h"
#include "net_types.h"
#include <sstream>
#include <fstream>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "stream_wrap.h"
#include <errno.h>

//@implements core

using namespace std;

namespace FD {

map<string,url_func>  &URLHandler::url_table()
{
  static map<string,url_func> table;
  return table;
}


ObjectRef file_url_handler(const string& url, int flags) {

  //FORMAT file:/home/..

  int _pos = url.find(":");
  
  string stripped_url = url.substr(_pos + 1, url.size());

  switch(flags) {

  case URLHandler::URL_READ:
  {
     ifstream *stream = new ifstream(stripped_url.c_str());
     if (stream->fail())
        throw new GeneralException("Cannot open file: " + stripped_url, __FILE__, __LINE__);
     else
        return ObjectRef(new IStream(stream));
  }
  break;

  case URLHandler::URL_WRITE:
  {
     ofstream *stream = new ofstream(stripped_url.c_str());
     if (stream->fail())
        throw new GeneralException("Cannot open file: " + stripped_url, __FILE__, __LINE__);
     else
        return ObjectRef(new OStream(stream));
  }
  break;

  case URLHandler::URL_READWRITE:
    {
     fstream *stream = new fstream(stripped_url.c_str());
     if (stream->fail())
        throw new GeneralException("Cannot open file: " + stripped_url, __FILE__, __LINE__);
     else
        return ObjectRef(new IOStream(stream));
  }
  break;

  default:
    ostringstream my_stream;
    my_stream<<"Unknown flag "<<flags<<" in file_url_handler";
    throw new GeneralException(my_stream.str(),__FILE__,__LINE__);
    break;
  }

  return nilObject;
}
REGISTER_URL_HANDLER(file,file_url_handler);


ObjectRef tcp_url_handler(const string& url, int flags) {

  //FORMAT tcp:hostname:port

  size_t _pos = url.find(":");

  string stripped_url = url.substr(_pos + 1, url.size());

  _pos = stripped_url.find(":");

  if (_pos != string::npos) {

    stripped_url[_pos] = ' ';
    istringstream my_stream(stripped_url);

    string host;
    int port;

    my_stream >> host;
    my_stream >> port;

    //Create the socket, code taken from TCPConnect

    struct sockaddr_in addr;
    int fd = socket(PF_INET, SOCK_STREAM, 0);
    //int port = 2313;
    struct hostent *entp;
   
    memset(&addr, 0, sizeof(struct sockaddr));
   
    addr.sin_family = AF_INET;
      
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(0);
    
    if (bind (fd, (struct sockaddr *)&addr, sizeof(addr)))
      throw new GeneralException(string("tcp_url_handler bind failed: ") + string(strerror(errno)), __FILE__, __LINE__);
    
    if((entp = gethostbyname(host.c_str())) == NULL)
      throw new GeneralException(string("tcp_url_handler Can't get host by name: ") + host, __FILE__, __LINE__);
      
    memcpy(&addr.sin_addr, entp->h_addr_list[0], entp->h_length);
    
    addr.sin_port = htons(port);
      
    if (connect (fd, (struct sockaddr *)&addr, sizeof(addr)))
      throw new GeneralException(string("tcp_url_handler connect failed: ") + string(strerror(errno)), __FILE__, __LINE__);
    
    //TODO do something for blocking / non blocking streams, include that into flags?

    /*
    if (!blocking)
      fcntl(fd, F_SETFL, O_NONBLOCK);
    */

    //create proper type of stream
    switch(flags) {
      
    case URLHandler::URL_READ:
      return ObjectRef (new IStream (new fd_istream(fd)));
      break;
      
    case URLHandler::URL_WRITE:
      return ObjectRef (new OStream (new fd_ostream(fd)));
      break;
      
    case URLHandler::URL_READWRITE:
      return ObjectRef (new IOStream (new fd_iostream(fd)));
      break;
      
    default:
      ostringstream my_stream;
      my_stream<<"Unknown flags "<<flags<<" in tcp_url_handler";
      throw new GeneralException(my_stream.str(),__FILE__,__LINE__);
      break;
    }
  }
  else {
    throw new GeneralException(string("no port specified for TCP URL : ") + stripped_url,__FILE__,__LINE__);
  }
  
  //should not happen...
  return nilObject;
}
REGISTER_URL_HANDLER(tcp,tcp_url_handler);

}//namespace FD
