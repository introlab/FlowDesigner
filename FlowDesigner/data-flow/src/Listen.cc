// Copyright (C) 2001 Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"
#include "Stream.h"
#include "SocketStream.h"

using namespace std;

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
 * @input_type socket
 *
 * @output_name SOCKET
 * @output_description The socket to be used for input/output operations
 * @output_type socket
 *
 * @parameter_name BACKLOG
 * @parameter_type int
 * @parameter_value 1
 * @parameter_description Number of incoming connections allowed
 *
 * @parameter_name BLOCKING
 * @parameter_type bool
 * @parameter_value true
 * @parameter_description Blocking call to accept.
 *
END*/


class Listen : public BufferedNode {
   
  int inputID;
  int outputID;
  int m_backlog;
  bool m_blocking;

public:

   Listen(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params), m_backlog(1), m_blocking(true) {

     inputID = addInput("SOCKET");
     outputID = addOutput("SOCKET");

     //getting parameters
     m_backlog = dereference_cast<int>(parameters.get("BACKLOG"));
     m_blocking = dereference_cast<bool>(parameters.get("BLOCKING"));
     

   }

   void calculate(int output_id, int count, Buffer &out) {


     
     ObjectRef socketValue = getInput(inputID,count);
                               
     iostream &my_stream =  (iostream&) object_cast<IOStream>(socketValue);

     socket_iostream *my_iostream = dynamic_cast<socket_iostream*>(&my_stream);

     if (my_iostream) {

       socket_streambuf &my_socket = (socket_streambuf&) (*my_iostream);

       if (my_socket.get_type() == network_socket::TCP_STREAM_TYPE) {	 
	 my_socket.socket_listen(m_backlog, m_blocking);
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
