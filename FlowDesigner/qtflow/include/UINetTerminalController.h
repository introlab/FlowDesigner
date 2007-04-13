#ifndef _UINETTERMINALCONTROLLER_H_
#define _UINETTERMINALCONTROLLER_H_

#include "UINetTerminal.h"
#include <QObject>

namespace FD {
	
	class UINetTerminalController : public QObject, public UINetTerminal
	{
		Q_OBJECT;
		public:
		
		
		private:
		UINetTerminalController();
	
	};

} //namespace FD


#endif