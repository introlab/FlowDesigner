#include "UITerminalController.h"
#include "UINodeController.h"
#include "QtNode.h"
#include "QtTerminal.h"
#include <iostream>

namespace FD
{
	using namespace std;
	
	
	UITerminalController::UITerminalController(ItemInfo *terminalInfo, UINodeController *_node, 
		bool _isInput, double _x, double _y)
		: UITerminal(terminalInfo,_node,_isInput,_x,_y), m_QtTerminal(NULL)
		
	{
            //UPDATE VIEW(S)
            if (terminalInfo && _node)
            {
                  
               QtNode* qtNode = _node->getQtNode();

               if (qtNode)
               {
                     m_QtTerminal = qtNode->addQtTerminal(this);
               }
               else 
               {
                     cerr<<"WARNING : UITerminalController::UITerminalController -- No QtNode defined."<<endl;
               }
            }
	}
	
	

} //namespace FD


