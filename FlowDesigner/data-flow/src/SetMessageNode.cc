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

class SetMessageNode; 

DECLARE_NODE(SetMessageNode) 
/*Node  
 *  
 * @name SetMessageNode
 * @category Mail
 * @description Mailbox to keep the newest data between loops
 *  
 * @input_name INPUT
 * @input_type ObjectRef
 * @input_description object to mail  
 *  
 * @output_name OUTPUT
 * @output_type ObjectRef
 * @output_description object to mail  
 *
 * @parameter_name MessageBox_Name
 * @parameter_type string
 * @parameter_description name  
 * 
END*/ 
class SetMessageNode : public BufferedNode {
       
	protected:
	   int input1ID;
	   int outputID;
	   std::string mailboxName;
	   MessageBox* mailbox;
	   
    public:
       SetMessageNode(string nodeName, ParameterSet params)
       : BufferedNode(nodeName, params)
       { 
          input1ID = addInput("INPUT");
		  outputID = addOutput("OUTPUT");
		  
		  //Get messagebox name
		  mailboxName = object_cast<String> ( parameters.get("MessageBox_Name"));
		  
		  //Get mailbox
		  MessageBoxManager* manager= MessageBoxManager::getInstance();
		  mailbox= manager->getMessageBoxFromName(mailboxName);
       } 

       void calculate(int output_id, int count, Buffer &out)
       { 
          //Get input data from previous node(s).*/
          ObjectRef input1Value = getInput(input1ID, count);
		  ObjectRef valueToSend = input1Value;
			if(DEBUG){
				cout<<'\n'<< "input1Value = "<< input1Value<<'\n';
				cout<<'\n'<< "set "<< valueToSend<<'\n';
		  }
		  mailbox->set(valueToSend);
			

		 out[count] = input1Value;
       }
     
};

}