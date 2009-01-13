/*
 *  MailBoxNode.cpp
 *  FlowDesigner
 *
 *  Created by Pascal Beaudry on 08-11-17.
 *  Copyright 2008 Laborius. All rights reserved.
 *
 */

#include "BufferedNode.h" 
#include "Node.h" 
#include "MailBox.h" 

namespace FD{
using namespace std;

class PopFromMailBoxNode; 

DECLARE_NODE(PopFromMailBoxNode) 
/*Node  
 *  
 * @name PopFromMailBoxNode
 * @category Mail
 * @description Read the last mail of the Mailbox
 *  
 * @output_name OUTPUT
 * @output_type ObjectRef
 * @output_description object to receive
 *
 * @parameter_name MailBox_Name
 * @parameter_type string
 * @parameter_description name  
END*/ 
PopFromMailBoxNode::PopFromMailBoxNode(string nodeName, ParameterSet params)
       : BufferedNode(nodeName, params)
       { 
		 outputID = addOutput("OUTPUT");
		  
		  //Get messagebox name
		  mailboxName = object_cast<String> ( parameters.get("MailBox_Name"));
		  

		  //Get mailbox
		  MailBoxManager* manager= MailBoxManager::getInstance();
		  mailbox= manager->getMailBoxFromName(mailboxName);
		  
		  //Register to mailbox
		  mailbox->registerReceiver(this);
		  
		  pthread_mutex_init(&pushmodifLock, NULL);
       }

void PopFromMailBoxNode::calculate(int output_id, int count, Buffer &out)
       { 
		  ObjectRef mailBoxData;
		  
		  while(msgQueue.empty()){
			if(DEBUG)
				cout<<'\n'<< "isEmpty"<<'\n';	
			thread_usleep(1500);	  
		  }
			mailBoxData =popObject(); 
			
		if(DEBUG)
			cout<<'\n'<< mailBoxData<<'\n';
		out[count] = mailBoxData;

       }
	   
void PopFromMailBoxNode::pushObject(ObjectRef obj){

	pthread_mutex_lock(&pushmodifLock);
	msgQueue.push(obj);
	pthread_mutex_unlock(&pushmodifLock);
}

ObjectRef PopFromMailBoxNode::popObject(){
	pthread_mutex_lock(&pushmodifLock);
	ObjectRef obj;
	obj=msgQueue.front();
	msgQueue.pop();
	pthread_mutex_unlock(&pushmodifLock);
	return obj;
}

}
