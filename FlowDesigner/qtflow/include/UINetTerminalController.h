#ifndef _UINETTERMINALCONTROLLER_H_
#define _UINETTERMINALCONTROLLER_H_

#include "UINetTerminal.h"

#include <QObject>

namespace FD {

        class QtNetTerminal;
        class QtTerminal;
        class UITerminalController;
	
	class UINetTerminalController : public QObject, public UINetTerminal
	{
		Q_OBJECT;
		public:
		
                UINetTerminalController(UITerminalController *_terminal, NetTermType _type, const std::string &_name, 
		 const std::string &_objType = "any", const std::string &_description = "No description available");
		
                void updateView(QtTerminal *terminal);

		private:
		

               QtNetTerminal *m_QtNetTerminal;
	};

} //namespace FD


#endif
