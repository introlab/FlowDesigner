// Copyright (C) 2002 Dominic Letourneau

#include "BufferedNode.h"
#include "net_types.h"
#include "Object.h"
#include <sstream>
#include <iostream>
#include "SocketStream.h"

using namespace std;

class BroadcastSave;
DECLARE_NODE(BroadcastSave)
/*Node
 *
 * @name BroadcastSave
 * @category IO
 * @description Takes an object and saves it using a stream, returns the input object
 *
 * @input_name OBJECT
 * @input_description The object that will be saved
 *
 * @input_name SOCKET
 * @input_description The output stream where to save
 * @input_type Stream
 *
 * @output_name OUTPUT
 * @output_description The input object
 *
END*/

class BroadcastSave : public BufferedNode {

protected:
   
   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'stream' input*/
   int socketInputID;

   /**The ID of the 'object' input*/
   int objectInputID;

   /**Reference to the opened stream*/
   ObjectRef openedFile;

public:
   BroadcastSave(string nodeName, ParameterSet params) 
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      socketInputID = addInput("SOCKET");
      objectInputID = addInput("OBJECT");
   }


   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef objectValue = getInput(objectInputID,count);
      Object &object = *objectValue;
      
      ostream & my_stream = (ostream&) object_cast<OStream>(getInput(socketInputID,count));

      socket_iostream *my_socket_iostream = dynamic_cast<socket_iostream*>(&my_stream);


      if (!my_socket_iostream) {
	throw new GeneralException("Invalid socket",__FILE__,__LINE__);
      }
      
      //writing to temporary stream
      ostringstream temp_stream;

      temp_stream << object;

      //writing directly to socket
      socket_streambuf &my_streambuf = (socket_streambuf&) (*my_socket_iostream);

      my_streambuf.send_packet((unsigned char*) temp_stream.str().c_str(), temp_stream.str().size());

      out[count] = objectValue;
   }
};



