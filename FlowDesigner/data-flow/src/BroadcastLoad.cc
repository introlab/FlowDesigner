// Copyright (C) 2001 Dominic Letourneau

#include "Vector.h"
#include "BufferedNode.h"
#include "ObjectRef.h"
#include "ObjectParser.h"
#include <sstream>
#include <strstream>
#include "SocketStream.h"

class BroadcastLoad;
DECLARE_NODE(BroadcastLoad)
/*Node
 *
 * @name BroadcastLoad
 * @category IO
 * @description Load an object from file (registered type)
 *
 * @input_name SOCKET
 * @input_description The stream we are loading from
 * @input_type Stream
 *
 * @output_name OUTPUT
 * @output_description The loaded object
 *
END*/


class BroadcastLoad : public BufferedNode {

protected:
   
  /**The ID of the 'output' output*/
  int outputID;

  /**The ID of the 'stream' input*/
  int socketInputID;
 
public:

  BroadcastLoad(string nodeName, ParameterSet params) 
     : BufferedNode(nodeName, params) {
    
    outputID = addOutput("OUTPUT");
    socketInputID = addInput("SOCKET");
    inOrder = true;
  }
  


  void calculate(int output_id, int count, Buffer &out) {

    strstream m_stream;

    istream & my_stream = (istream&) object_cast<IStream>(getInput(socketInputID,count));
    
    socket_iostream *my_socket_iostream = dynamic_cast<socket_iostream*>(&my_stream);
    
    if (!my_socket_iostream) {
      throw new GeneralException("Invalid socket",__FILE__,__LINE__);
    }
    
    //getting socket
    socket_streambuf &my_streambuf = (socket_streambuf&) (*my_socket_iostream);


    Vector<ObjectRef> *output_vect = new Vector<ObjectRef>;

    int object_count = 0;

    while(1) {

      //32 kb of data
      unsigned char packet[1024 * 32];
      
      memset(&packet[0],0,1024 * 32);
      
      int size;
      
      size = my_streambuf.recv_packet(&packet[0], 1024 * 32);
      
      if (size > 0) {
	//cerr<<"BROADCAST LOAD SIZE : "<<size<<endl;
	m_stream.write(&packet[0],size);
	ObjectRef my_object;      
	
	try {
	  m_stream >> my_object;      
	  output_vect->push_back(my_object);
	  object_count++;
	}
	catch(BaseException *e) {
	  cerr<<"BroadcastLoad (exception)"<<endl;
	  e->print(cerr);
	  delete e;
	}
      }
      else {
	break;
      }
    }  

   
    //cerr<<"BroadcastLoad read : "<<object_count<<" objects!"<<endl;
    
    out[count] = ObjectRef(output_vect);
    
   }

};
