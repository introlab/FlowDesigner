// Copyright (C) 2004 Jean-Marc Valin

#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <string.h>
#include "BufferedNode.h"
#include "stream_wrap.h"
#include "Stream.h"

class TCPConnect;

DECLARE_NODE(TCPConnect)
/*Node
 *
 * @name TCPConnect
 * @category IO
 * @description Creates a read-only stream from a filename
 *
 * @output_name OUTPUT
 * @output_description Commected socket in RW mode
 * @output_type Stream
 *
 * @parameter_name HOST
 * @parameter_type string
 * @parameter_description Hostname
 *
 * @parameter_name PORT
 * @parameter_type int
 * @parameter_description Port number
 *
 * @parameter_name BLOCKING
 * @parameter_type bool
 * @parameter_value true
 * @parameter_description Whether to make the socket blocking
 *
END*/

class TCPConnect : public BufferedNode {

   int outputID;
   string host;
   int port;
   bool blocking;
   
public:
   TCPConnect(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      
      host = object_cast<String> (parameters.get("HOST"));
      
      port = dereference_cast<int> (parameters.get("PORT"));
      
      if (parameters.exist("BLOCKING"))
         blocking = dereference_cast<bool> (parameters.get("BLOCKING"));
      else
         blocking = true; 
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      struct sockaddr_in addr;
      int fd = socket(PF_INET, SOCK_STREAM, 0);
      //int port = 2313;
      struct hostent *entp;
   
      memset(&addr, 0, sizeof(struct sockaddr));
   
      addr.sin_family = AF_INET;
      
      addr.sin_addr.s_addr = htonl(INADDR_ANY);
      addr.sin_port = htons(0);
      
      if (bind (fd, (struct sockaddr *)&addr, sizeof(addr)))
         throw new NodeException(this, string("bind failed: ") + string(strerror(errno)), __FILE__, __LINE__);

      if((entp = gethostbyname(host.c_str())) == NULL)
         throw new NodeException(this, string("Can't get host by name: ") + host, __FILE__, __LINE__);
      
      memcpy(&addr.sin_addr, entp->h_addr_list[0], entp->h_length);
   
      addr.sin_port = htons(port);
      
      if (connect (fd, (struct sockaddr *)&addr, sizeof(addr)))
         throw new NodeException(this, string("connect failed: ") + string(strerror(errno)), __FILE__, __LINE__);
      
      if (!blocking)
         fcntl(fd, F_SETFL, O_NONBLOCK);
      
      out[count] = ObjectRef (new IOStream (new fd_iostream(fd)));
   }

   NO_ORDER_NODE_SPEEDUP(TCPConnect)
};



