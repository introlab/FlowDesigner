// Copyright (C) 2001 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include "NetworkSocket.h"

class Accept;

DECLARE_NODE(Accept)
/*Node
 *
 * @name Accept
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


class Accept : public BufferedNode {
   
  int inputID;
  int outputID;

public:

   Accept(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params) {

     inputID = addInput("SOCKET");
     outputID = addOutput("SOCKET");
   }

   void calculate(int output_id, int count, Buffer &out) {


     ObjectRef socketValue = getInput(inputID,count);

     NetworkSocket &my_socket = object_cast<NetworkSocket>(socketValue);

     if (my_socket.get_type() == NetworkSocket::TCP_STREAM_TYPE) {

       my_socket.socket_accept();

     }
     else {
       throw new GeneralException("NetworkSocket is not of type TCP_STREAM_TYPE",__FILE__,__LINE__);
     }

     out[count] = socketValue;

   }

};