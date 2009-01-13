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


namespace FD {

using namespace std;

class GetMessageNode; 


DECLARE_NODE(GetMessageNode) 
/*Node  
 *  
 * @name GetMessageNode
 * @category Mail
 * @description Mailbox to keep the newest data between loops
 *  
 * @output_name OUTPUT
 * @output_type ObjectRef
 * @output_description object to receive
 *
 * @parameter_name MessageBox_Name
 * @parameter_type string
 * @parameter_description name  
 * 
END*/ 
    class GetMessageNode : public BufferedNode {

       int outputID;
	   std::string mailboxName;
	   MessageBox* mailbox;
    public:
       GetMessageNode(string nodeName, ParameterSet params)
       : BufferedNode(nodeName, params)
       { 
		 outputID = addOutput("OUTPUT");
		  
		  //Get messagebox name
		  mailboxName = object_cast<String> ( parameters.get("MessageBox_Name"));
		  
		  //Get mailbox
		  MessageBoxManager* manager= MessageBoxManager::getInstance();
		  mailbox= manager->getMessageBoxFromName(mailboxName);
       } 

       void calculate(int output_id, int count, Buffer &out)
       { 
		  ObjectRef mailBoxData;
		  
		  while(mailbox->isEmpty()){
		  if(DEBUG)
			cout<<'\n'<< "isEmpty"<<'\n';	 
		  }
			mailBoxData = (mailbox->get());
			
		if(DEBUG)
			cout<<'\n'<< mailBoxData<<'\n';
			out[count] = mailBoxData;

       }
     
};

}
