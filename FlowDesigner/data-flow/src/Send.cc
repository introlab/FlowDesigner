// Copyright (C) 2000 Jean-Marc Valin & Dominic Letourneau


#include "Node.h"
#include "ObjectParser.h"
#include <iostream>
#include <sys/types.h> 
#include <sys/socket.h> 
#include <netinet/in.h>

using namespace std;

#define LOOPBACK "127.0.0.1"

class Send;

DECLARE_NODE(Send)
/*Node

 * @name Send
 * @category General
 * @description Send data through network (Not Working Yet)

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name VALUE
 * @parameter_description No description available
 * @parameter_type string

END*/


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
