// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


#include "Node.h"
#include "ObjectParser.h"
#include <stream.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include <string.h>

class Receive;

DECLARE_NODE(Receive)
/*Node

 * @name Receive
 * @category IO
 * @description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name VALUE
 * @parameter_description No description available

END*/


/** A constant node contains a value that will never changes. */
class Receive : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /**The ID of the 'value' output*/
   int outputID;

    int sock;
    struct sockaddr_in addr;

public:

   /**Constructor, takes the name of the node and a set of parameters*/
   Receive(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      //, value (parameters.get("VALUE"))
   {
      outputID = addOutput("OUTPUT");
      
      cerr << "setting addr\n";
      addr.sin_family = AF_INET;
      addr.sin_port = htons (5248);
      struct hostent *hostinfo;
      hostinfo = gethostbyname ("localhost");
      addr.sin_addr = *(struct in_addr *) hostinfo->h_addr;
      
      cerr << "creating socket\n";
      sock = socket (AF_INET, SOCK_STREAM, 6);

      cerr << "calling bind\n";
      if (bind(sock, (struct sockaddr *) &addr, sizeof(addr)))
      {
	 perror ("error opening socket");
	 throw new NodeException(NULL, "can't connect socket",__FILE__,__LINE__);
      }

      cerr << "calling connect\n";
      if (connect(sock, (struct sockaddr *) &addr, sizeof(addr)))
      {
	 perror ("error opening socket");
	 throw new NodeException(NULL, "can't connect socket",__FILE__,__LINE__);
      }
      cerr << "socket opened\n";
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return Object::nilObject;
      else throw new NodeException (this, "Receive: Unknown output id", __FILE__, __LINE__);
   }

protected:
   /**Default constructor, should not be used*/
   Receive() {throw new GeneralException("Receive copy constructor should not be called",__FILE__,__LINE__);}

};
