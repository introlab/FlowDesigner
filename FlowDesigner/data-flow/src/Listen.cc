// Copyright (C) 2001 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include "NetworkSocket.h"

class Listen;

DECLARE_NODE(Listen)
/*Node
 *
 * @name Listen
 * @category Network
 * @description Create a network socket of any type
 *
 * @input_name SOCKET
 * @input_description The socket to listen to
 *
 * @output_name SOCKET
 * @output_description The socket to be used for input/output operations
 *
END*/


class Listen : public BufferedNode {
   
  int inputID;
  int outputID;

public:

   Listen(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     inputID = addInput("SOCKET");
     outputID = addOutput("SOCKET");
   }

   void calculate(int output_id, int count, Buffer &out) {


     ObjectRef socketValue = getInput(inputID,count);

     NetworkSocket &my_socket = object_cast<NetworkSocket>(socketValue);

     if (my_socket.get_type() == NetworkSocket::TCP_SERVER_STREAM_TYPE) {

       //backlog = 1
       my_socket.server_listen(1);

       my_socket.server_accept();

     }
     else {
       throw new GeneralException("NetworkSocket is not of type TCP_SERVER_STREAM_TYPE",__FILE__,__LINE__);
     }

     out[count] = socketValue;

   }

};
