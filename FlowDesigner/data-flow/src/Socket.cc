// Copyright (C) 2001 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include "NetworkSocket.h"

class Socket;

DECLARE_NODE(Socket)
/*Node
 *
 * @name Socket
 * @category Network
 * @description Create a network socket of any type
 *
 * @parameter_name TYPE
 * @parameter_type String
 * @parameter_description Type of socket : BROADCAST, etc.
 *
 * @parameter_name PORT
 * @parameter_type int
 * @parameter_description Communication port
 *
 * @output_name OUTPUT
 * @output_description The socket to be used for input/output operations
 *
END*/


class Socket : public BufferedNode {
   
  int outputID;

  ObjectRef m_socketRef;

public:

   Socket(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     const String &socketType = object_cast<String> (parameters.get("TYPE"));
     int port = dereference_cast<int> (parameters.get("PORT"));
     
     m_socketRef = nilObject;

     if (socketType == "BROADCAST") {
       m_socketRef = ObjectRef(new NetworkSocket(NetworkSocket::BROADCAST_TYPE, port));
     }

     outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out) {
     out[count] = m_socketRef;
   }

};
