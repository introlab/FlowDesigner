#ifndef _UITERMINALCONTROLLER_H_
#define _UITERMINALCONTROLLER_H_

#include "UITerminal.h"
#include <QObject>

namespace FD {
	
	class UITerminalController : public QObject, public UITerminal
	{
		Q_OBJECT;
		public:
		
		
		private:
		UITerminalController();
	
	};

} //namespace FD


#endif
