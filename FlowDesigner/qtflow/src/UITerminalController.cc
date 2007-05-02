#include "UITerminalController.h"
#include "UINetTerminalController.h"
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
                     m_QtTerminal = qtNode->addTerminal(this);
               }
               else 
               {
                     cerr<<"WARNING : UITerminalController::UITerminalController -- No QtNode defined."<<endl;
               }
            }
	}
	
        QtTerminal* UITerminalController::getQtTerminal()
        {
            return m_QtTerminal;         
        }
	
        void UITerminalController::updateView(QtNode *node)
        {
            //CREATE VIEW IF REQUIRED
            if (!m_QtTerminal && node)
            {
                m_QtTerminal = node->addTerminal(this);
            }
    
            //UPDATE NET TERMINAL
            if (netTerminal)
            {
                UINetTerminalController *netTerminalCTRL = dynamic_cast<UINetTerminalController*>(netTerminal);

                if (netTerminalCTRL)
                {
                    netTerminalCTRL->updateView(m_QtTerminal);
                }
                

            }


        }

} //namespace FD


