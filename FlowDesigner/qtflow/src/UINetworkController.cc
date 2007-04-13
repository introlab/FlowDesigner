
#include "UINetworkController.h"

namespace FD {

	UINetworkController::UINetworkController()
		:	UINetwork(NULL,NULL,false)
	{
	
	
	}
	
	UINode* UINetworkController::newNode(UINetwork* _net, std::string _name, 
						   std::string _type, double _x, double _y, bool doInit)
	{
	
	}
						   
	UILink* UINetworkController::newLink (UITerminal *_from, UITerminal *_to,const char *str)
	{
	
	}
		
	UINote* UINetworkController::newNote(const std::string &text, double x, double y, bool visible)
	{
	
	
	}

	UINetTerminal * UINetworkController::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name, 
					  const std::string &_objType, const std::string &_description)
	{
					  
	}

} //namespace FD

