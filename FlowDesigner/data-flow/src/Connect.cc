// Copyright (C) 2001 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include "SocketStream.h"

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
 * @input_type socket
 *
 * @input_name HOST
 * @input_description The host we want to connect to.
 *
 * @output_name SOCKET
 * @output_description The socket to be used for input/output operations
 * @output_type stream
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
     
     
     iostream &my_stream =  (iostream&) object_cast<IOStream>(socketValue);
     socket_iostream *my_iostream = dynamic_cast<socket_iostream*>(&my_stream);
     
     if (my_iostream) {
       
       socket_streambuf &my_socket = (socket_streambuf&) (*my_iostream);

       if (my_socket.get_type() == network_socket::TCP_STREAM_TYPE) {
 
	 const String &hostname = object_cast<String>(hostValue);	 
	 my_socket.socket_connect(hostname.c_str());
       
       }
       else {
	 throw new GeneralException("Socket is not of type TCP_STREAM_TYPE.",__FILE__,__LINE__);
       }
     }
     else {
       throw new GeneralException("Unable to get network_socket pointer.",__FILE__,__LINE__);
     }

     out[count] = socketValue;

   }

};
