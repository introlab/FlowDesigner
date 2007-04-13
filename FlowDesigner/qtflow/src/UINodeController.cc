
#include "UINodeController.h"

namespace FD
{
	using namespace std;

	UINodeController::UINodeController()
		: UINode(NULL,NULL,false)
	{
	
	
	}
	
	
	UILink* UINodeController::newLink (UITerminal *_from, UITerminal *_to)
	{
	
	}

	UINetTerminal* UINodeController::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name,
		const std::string &_objType, const std::string &_description)
	{
	
	}

	UINodeParameters* UINodeController::newNodeParameters (UINode *_node, std::string type)
	{
	
	
	}

	void UINodeController::rename (const std::string &newName)
	{
	
	
	}

}