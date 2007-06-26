
#include "UINodeController.h"
#include "UINetworkController.h"
#include "UITerminalController.h"
#include "UINetTerminalController.h"
#include "UIDocumentController.h"
#include "UILinkController.h"
#include "UILink.h"
#include "UINodeParameters.h"

#include "QtNode.h"
#include "QtNetwork.h"
#include <iostream>

namespace FD
{
	using namespace std;

	UINodeController::UINodeController()
		: UINode(NULL,NULL,false), m_QtNode(NULL)
	{
	
	
	}
	
	UINodeController::UINodeController(UINetworkController* _net, xmlNodePtr def)
		: UINode(_net,def,false), m_QtNode(NULL)
	{
        cerr<<"UINodeController::UINodeController(UINetworkController* _net, xmlNodePtr def)"<<endl;

        //creating GUI Node
        if (_net && def)
        {
            //loading component
            loadXML(def);
        }



	}
	
	
	UINodeController::UINodeController(UINetworkController* _net, std::string _name, std::string _type, double x, double y)
		: UINode(_net,_name,_type,x,y,false), m_QtNode(NULL)
	{
		cerr<<"UINodeController::UINodeController created"<<endl;
		description = net->getDocument()->getDescription(type);
		
                //Updating information from terminals
                updateTerminals();

                //updating information from parameters
                updateParameters();

	}
	
        void UINodeController::updateTerminals()
        {
        //WARNING : 
		//This part is taken from  UINode's constructor and must be reimplemented here since
		//it calls virtual functions that are not "known" to base class at construction.

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
        }

        void UINodeController::updateParameters()
        {
                //WARNING : 
		//This part is taken from  UINode's constructor and must be reimplemented here since
		//it calls virtual functions that are not "known" to base class at construction.		
		parameters = newNodeParameters(this,type);

        }

	
	UILink* UINodeController::newLink (UITerminal *_from, UITerminal *_to)
	{
		cerr<<"UILink* UINodeController::newLink"<<endl;
		return new UILinkController(dynamic_cast<UITerminalController*>(_from),dynamic_cast<UITerminalController*>(_to));
	}

	
	UITerminal* UINodeController::newTerminal(ItemInfo *_info, UINode *_node, bool _isInput, double _x, double _y)
	{
		cerr<<"UITerminal* UINodeController::newTerminal"<<endl;
		return new UITerminalController(_info,dynamic_cast<UINodeController*>(_node),_isInput,_x,_y);
	}
	
	UINetTerminal* UINodeController::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name,
		const std::string &_objType, const std::string &_description)
	{
	    cerr<<"UINetTerminal* UINodeController::newNetTerminal"<<endl;
	    return  new UINetTerminalController(dynamic_cast<UITerminalController*>(_terminal), _type, _name);		
	}

	UINodeParameters* UINodeController::newNodeParameters (UINode *_node, std::string type)
	{
		cerr<<"UINodeParameters* UINodeController::newNodeParameters"<<endl;
		//return new UINodeParametersController(dynamic_cast<UINodeController*>(_node),type);
		return new UINodeParameters(_node,type);
	}

	void UINodeController::rename (const std::string &newName)
	{
		UINode::rename(newName);
	}
      
    void UINodeController::setQtNode(QtNode* node)
     {
        cerr<<"UINodeController::setQtNode "<<node<<endl;
        m_QtNode = node;
     }

    void UINodeController::updateView(QtNetwork *net)
    {
        //CREATE THE VIEW IF REQUIRED
        if (!m_QtNode && net)
        {
               m_QtNode = net->addNode(this);

               connect(m_QtNode,SIGNAL(positionChanged(float, float)),this,SLOT(setPos(float, float)));
        }

        //UPDATE THE VIEW
        //INPUT TERMINALS
        for (unsigned int i = 0; i < inputs.size(); i++)
        {
            UITerminalController *terminalCTRL = dynamic_cast<UITerminalController*>(inputs[i]); 

            if (terminalCTRL)
            {
                terminalCTRL->updateView(m_QtNode);
            }
           
        }
        //OUTPUT TERMINALS
        for (unsigned int i = 0; i < outputs.size(); i++)
        {
            UITerminalController *terminalCTRL = dynamic_cast<UITerminalController*>(outputs[i]);

            if (terminalCTRL)
            {
                terminalCTRL->updateView(m_QtNode);
            }
        }
	
	//TODO : LINKS


    }

    void UINodeController::setPos(float _x, float _y)
    {

        cerr<<"(SLOT) UINodeController::setPos(float _x, float _y)"<<_x<<","<<_y<<endl;

        x = _x;
        y = _y;
        
        //update view

        
    }
}
