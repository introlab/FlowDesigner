// Copyright (C) 2001 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include "NetworkSocket.h"

class Connect;

DECLARE_NODE(Connect)
/*Node
 *
 * @name Connect
 * @category Network
 * @description Create a network socket of any type
 *
 * @input_name SOCKET
 * @input_description The socket to listen to
 *
 * @input_name HOST
 * @input_description The host we want to connect to.
 *
 * @output_name SOCKET
 * @output_description The socket to be used for input/output operations
 *
END*/


class Connect : public BufferedNode {
   
  int inputID;
  int outputID;
  int hostID;

public:

   Connect(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     inputID = addInput("SOCKET");
     hostID = addInput("HOST");
     outputID = addOutput("SOCKET");
   }

   void calculate(int output_id, int count, Buffer &out) {


     ObjectRef socketValue = getInput(inputID,count);
     ObjectRef hostValue = getInput(hostID,count);

     NetworkSocket &my_socket = object_cast<NetworkSocket>(socketValue);

     if (my_socket.get_type() == NetworkSocket::TCP_CLIENT_STREAM_TYPE) {


       const String &hostname = object_cast<String>(hostValue);

       my_socket.client_connect(hostname.c_str());


     }
     else {
       throw new GeneralException("NetworkSocket is not of type TCP_CLIENT_STREAM_TYPE",__FILE__,__LINE__);
     }

     out[count] = socketValue;

   }

};
