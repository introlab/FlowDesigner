
#include "UINetworkController.h"
#include "UIDocumentController.h"
#include "UINodeController.h"
#include "UITerminalController.h"
#include "UINetTerminalController.h"
#include "UILinkController.h"
#include <iostream>
#include "QtNetwork.h"
#include "UILink.h"
#include "UINote.h"

namespace FD {
    
	using namespace std;
    
	UINetworkController::UINetworkController()
    :	UINetwork(NULL,NULL,false), m_QtNetwork(NULL)
	{
        
        
	}
    
	UINetworkController::UINetworkController(UIDocumentController *_doc, xmlNodePtr net)
    : UINetwork(_doc,net,false), m_QtNetwork(NULL)
	{
		//Load XML		
		load(net);	
	}
	
	UINetworkController::UINetworkController(UIDocumentController *doc, const std::string &_name, UINetwork::Type type)
    : UINetwork(doc,_name,type), m_QtNetwork(NULL)
	{
        
        
	}
	
	UINode* UINetworkController::newNode(UINetwork* _net, std::string _name, 
    std::string _type, double _x, double _y, bool doInit)
	{
        
		cerr<<"UINode* UINetworkController::newNode "<<endl;
        getDocument()->updateAllNetworks();
		//CREATE MODEL & CONTROLLER
		UINodeController *nodeController = new UINodeController(dynamic_cast<UINetworkController*>(_net),_name,_type,_x,_y);
        
		//UPDATE VIEW
		//TO DO, UPDATE MULTIPLE VIEWS?
		if (m_QtNetwork)
		{
			//QtNode *qtNode = m_QtNetwork->addNode(nodeController);
            //nodeController->setQtNode(qtNode);
		}
        
        nodeController->updateView(m_QtNetwork);	
        
        
        //UPDATE VIEW 
        //nodeController->updateTerminals();
        //nodeController->updateParameters();
        
        
		return nodeController;
	}
    
    UINode* UINetworkController::newNode(UINetwork* _net, xmlNodePtr def)
    {
		cerr<<"UINode* UINetworkController::newNode (XML)"<<endl;
        
		//CREATE MODEL & CONTROLLER
		UINodeController *nodeController = new UINodeController(dynamic_cast<UINetworkController*>(_net),def);
        
		//UPDATE VIEW
		//TO DO, UPDATE MULTIPLE VIEWS?
		if (m_QtNetwork)
		{
            nodeController->updateView(m_QtNetwork);
			//QtNode *qtNode = m_QtNetwork->addNode(nodeController);
            //nodeController->setQtNode(qtNode);
		}
        else
        {
            cerr<<"WARNING :  UINode* UINetworkController::newNode --> No QtNetwork defined."<<endl;
        }
        
        //UPDATE VIEW 
        //nodeController->updateTerminals();
        //nodeController->updateParameters();
        
        return nodeController;
    }
    
    
    
	UILink* UINetworkController::newLink (UITerminal *_from, UITerminal *_to,const char *str)
	{
		cerr<<"UILink* UINetworkController::newLink"<<endl;
		
		//CREATE MODEL & CONTROLLER
		//UILinkController *linkController = new UILinkController(dynamic_cast<UITerminalController*>(_from),
		//														);
        UILinkController *linkController = new UILinkController(dynamic_cast<UITerminalController*>(_from),dynamic_cast<UITerminalController*>(_to),str);
        linkController->updateView( m_QtNetwork );
		return linkController;
	}
    
	UINote* UINetworkController::newNote(const std::string &text, double x, double y, bool visible)
	{
		return new UINote(text,x,y,visible);	
	}
    
	UINetTerminal * UINetworkController::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name, 
    const std::string &_objType, const std::string &_description)
	{
		return new UINetTerminalController(dynamic_cast<UITerminalController*>(_terminal),_type,_name,_objType,_description);		  
	}
	
	UINodeController* UINetworkController::createNode(std::string type, double x, double y, bool doInit)
	{
		UINodeController* myNode = dynamic_cast<UINodeController*>(newNode(this,"name",type,x,y,doInit));
		addNode(myNode);	
		return myNode;
	}
    
    void UINetworkController::removeNode(UINodeController* node)
    {        
        cerr<<"UINetworkController::removeNode(UINodeController* node)"<<endl;
        std::vector<UITerminal*> terminal = node->getInputs();
        for( unsigned int i = 0; i < terminal.size();i++ )
        {       
            node->removeTerminal( dynamic_cast<UITerminalController*>(terminal[i]) );
        }
        terminal = node->getOutputs();
        for( unsigned int i = 0; i < terminal.size();i++ )
        {       
            node->removeTerminal( dynamic_cast<UITerminalController*>(terminal[i]) );
        }
        
        m_QtNetwork->removeNode(node->getQtNode());
        UINetwork::removeNode(node);
        cerr<<"UINetworkController::removeNode(UINodeController* node) END"<<endl;
    }
    
    void UINetworkController::removeLink(UILinkController* link)
    {
        m_QtNetwork->removeLink(link->getQtLink());
        UINetwork::removeLink(link);
    }
	
    void UINetworkController::updateView(QtDocument *doc)
    {        
        getDocument()->updateAllNetworks();
        if (doc)
        {
            //CREATE VIEW IF REQUIRED
            if (!m_QtNetwork)
            {
                m_QtNetwork = doc->addNetwork(this);
            }
            
            //UPDATE VIEW FOR EACH NODES
            for (unsigned int i =0; i < nodes.size(); i++)
            {
                UINodeController *nodeCtrl = dynamic_cast<UINodeController*>(nodes[i]);
                
                if (nodeCtrl)
                {                    
                    nodeCtrl->updateView(m_QtNetwork);
                }
                
            }
            
            //UPDATE VIEW FOR EACH LINKS
            for (unsigned int i = 0; i < links.size(); i++)
            {
                UILinkController *linkCtrl = dynamic_cast<UILinkController*>(links[i]);
                
                if (linkCtrl)
                {
                    linkCtrl->updateView(m_QtNetwork);
                }
                
            }
        }//if doc
    }
    
} //namespace FD

