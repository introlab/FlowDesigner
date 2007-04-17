
#include "UINetworkController.h"
#include "UIDocumentController.h"
#include "UINodeController.h"
#include <iostream>
#include "QtNetwork.h"

namespace FD {

	using namespace std;

	UINetworkController::UINetworkController()
		:	UINetwork(NULL,NULL,false)
	{
	
	
	}
	
	UINetworkController::UINetworkController(UIDocumentController *doc, const std::string &_name, UINetwork::Type type)
		: UINetwork(doc,_name,type), m_QtNetwork(NULL)
	{
	
	
	}
	
	UINode* UINetworkController::newNode(UINetwork* _net, std::string _name, 
						   std::string _type, double _x, double _y, bool doInit)
	{
	
		cerr<<"UINode* UINetworkController::newNode "<<endl;
	
		//CREATE MODEL & CONTROLLER
		UINodeController *nodeController = new UINodeController(dynamic_cast<UINetworkController*>(_net),_name,_type,_x,_y);
	
		//UPDATE VIEW
		//TO DO, UPDATE MULTIPLE VIEWS?
		if (m_QtNetwork)
		{
			QtNode *qtNode = m_QtNetwork->addNode(nodeController);
                        nodeController->setQtNode(qtNode);
		}
	
                //UPDATE VIEW 
               nodeController->updateTerminals();
               nodeController->updateParameters();


		return nodeController;
	}
						   
	UILink* UINetworkController::newLink (UITerminal *_from, UITerminal *_to,const char *str)
	{
		cerr<<"UILink* UINetworkController::newLink"<<endl;
		
		//CREATE MODEL & CONTROLLER
		//UILinkController *linkController = new UILinkController(dynamic_cast<UITerminalController*>(_from),
		//														);
		
		
		
		return NULL;
	}
		
	UINote* UINetworkController::newNote(const std::string &text, double x, double y, bool visible)
	{
		return NULL;	
	}

	UINetTerminal * UINetworkController::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name, 
					  const std::string &_objType, const std::string &_description)
	{
		return NULL;		  
	}
	
	UINodeController* UINetworkController::createNode(std::string type, double x, double y, bool doInit)
	{
		UINodeController* myNode = dynamic_cast<UINodeController*>(newNode(this,"name",type,x,y,doInit));
		addNode(myNode);	
		return myNode;
	}
	

} //namespace FD

