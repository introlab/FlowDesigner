// Copyright (C) 2000 Jean-Marc Valin & Dominic Letourneau
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
#include <strstream.h>
#include <sys/types.h> 
#include <sys/socket.h> 
#include <netinet/in.h>

#define LOOPBACK "127.0.0.1"

class Send;

//DECLARE_NODE(Send)
NODE_INFO(Send,"General", "", "OUTPUT", "VALUE")

/** A constant node contains a value that will never changes. */
class Send : public Node
{

protected:

  struct sockaddr_in m_name;
  struct sockaddr_in m_clientname;

  /**The value of the constant*/
  ObjectRef value;
  
  int m_socket;
  
  /**The ID of the 'value' output*/
  int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   Send(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      //, value (parameters.get("VALUE"))
   {
      outputID = addOutput("OUTPUT");
      m_socket = socket(PF_INET,SOCK_STREAM,6);
      if (m_socket == -1) {
	throw new NodeException(NULL, "can't create socket",__FILE__,__LINE__);
      }
      
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {

     char in_buffer[2048];

     while (1) {

       m_name.sin_family = AF_INET;
       m_name.sin_port = htons (5248);
       m_name.sin_addr.s_addr = htonl (INADDR_ANY);
       

       cout<<"bind"<<endl;
       //bind (m_socket, (struct sockaddr *) &m_name, sizeof (m_name));
       
       if ((bind (m_socket, (struct sockaddr *) &m_name, sizeof (m_name))) < 0);
       {
	 perror ("error binding socket");
	 throw new NodeException(NULL, "can't bind socket",__FILE__,__LINE__);
       }
            
       cout<<"listen"<<endl;
       //listen (m_socket, 1);
       
       if ((listen (m_socket, 1)) < 0) {
	 perror ("error listening socket");
	 throw new NodeException(NULL, "can't listen socket",__FILE__,__LINE__);
       }
	 
       int size;

       cout<<"accept"<<endl;
       int conn;
       //conn = accept (m_socket,(struct sockaddr *) &m_clientname,&size);


       cout<<"recv"<<endl;
       int data_length = recv(conn,in_buffer,2048,NULL);
       cout<<"got length : "<<data_length<<endl;

       cout<<"got this :";
       for (int i=0; i< data_length;i++) {
	 cout<<in_buffer[i];
	 
       }
       cout<<endl;

       //should compute here!

       //returns the value
       strcpy(in_buffer,"ALLO\0");

       cout<<"sending"<<endl;
       send(conn,in_buffer,strlen(in_buffer), NULL);
	 
       
      
     }
   }

protected:
   /**Default constructor, should not be used*/
   Send() {throw new GeneralException("Send copy constructor should not be called",__FILE__,__LINE__);}

};
