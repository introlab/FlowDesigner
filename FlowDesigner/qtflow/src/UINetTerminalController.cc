#include "UINetTerminalController.h"
#include "UITerminalController.h"
#include "QtTerminal.h"
#include "QtNetTerminal.h"
#include <iostream>

namespace FD
{
	using namespace std;
	

         UINetTerminalController::UINetTerminalController(UITerminalController *_terminal, NetTermType _type, const std::string &_name, const std::string &_objType, const std::string &_description)
          : UINetTerminal(_terminal,_type,_name,_objType,_description), m_QtNetTerminal(NULL)
         {

            QtTerminal *qtTerminal = _terminal->getQtTerminal();

            //UPDATE VIEW(S)
            if (qtTerminal)
            {
                  m_QtNetTerminal = qtTerminal->addNetTerminal(this);
            }
            else
            {
               cerr<<"WARNING UINetTerminalController::UINetTerminalController -->QtTerminal not set"<<endl;
            }    
         }

        void UINetTerminalController::updateView(QtTerminal *terminal)
        {            
            //CREATE VIEW IF REQUIRED
            if (!m_QtNetTerminal && terminal)
            {              
                m_QtNetTerminal = terminal->addNetTerminal(this);
            }
        }
}
