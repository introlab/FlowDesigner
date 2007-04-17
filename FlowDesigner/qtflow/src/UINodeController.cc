
#include "UINodeController.h"
#include "UINetworkController.h"
#include "UITerminalController.h"
#include "UIDocumentController.h"
#include "QtNode.h"
#include <iostream>

namespace FD
{
	using namespace std;

	UINodeController::UINodeController()
		: UINode(NULL,NULL,false), m_QtNode(NULL)
	{
	
	
	}
	
	UINodeController::UINodeController(UINetworkController* _net, std::string _name, std::string _type, double x, double y)
		: UINode(_net,_name,_type,x,y,false), m_QtNode(NULL)
	{
		cerr<<"UINodeController::UINodeController created"<<endl;

		//WARNING : 
		//This part is taken from  UINode's constructor and must be reimplemented here since
		//it calls virtual functions that are not "known" to base class at construction.		
		parameters = newNodeParameters(this,type);

		vector<ItemInfo *> inputname;
		vector<ItemInfo *> outputname;
		inputname = net->getDocument()->getNetInputs(type); 
		outputname = net->getDocument()->getNetOutputs(type); 
	  
		for (unsigned int i=0;i<inputname.size();i++)
		{
			inputs.insert(inputs.end(), newTerminal(inputname[i], this, true, 0.0, 0.0));
		}
	  
		for (unsigned int i=0;i<outputname.size();i++) 
		{ 
			outputs.insert(outputs.end(), newTerminal(outputname[i], this, false, 0.0, 0.0));
		}
	  
		description = net->getDocument()->getDescription(type);
		//END  
		  
		
		
	}
	
	
	UILink* UINodeController::newLink (UITerminal *_from, UITerminal *_to)
	{
		return NULL;
	}

	
	UITerminal* UINodeController::newTerminal(ItemInfo *_info, UINode *_node, bool _isInput, double _x, double _y)
	{
		cerr<<"UITerminal* UINodeController::newTerminal"<<endl;
		return new UITerminalController(_info,dynamic_cast<UINodeController*>(_node),_isInput,_x,_y);
	}
	
	UINetTerminal* UINodeController::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name,
		const std::string &_objType, const std::string &_description)
	{
		return NULL;
	}

	UINodeParameters* UINodeController::newNodeParameters (UINode *_node, std::string type)
	{
		return NULL;	
	}

	void UINodeController::rename (const std::string &newName)
	{
		
	
	}

}