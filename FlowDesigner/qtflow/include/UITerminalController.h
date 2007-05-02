#ifndef _UITERMINALCONTROLLER_H_
#define _UITERMINALCONTROLLER_H_

#include "UITerminal.h"
#include <QObject>

namespace FD {
	
	class UINodeController;
    class QtTerminal;
    class QtNode;
	
	class UITerminalController : public QObject, public UITerminal
	{
		Q_OBJECT;

		public:
		
		UITerminalController(ItemInfo *terminalInfo, UINodeController *_node, bool _isInput, double _x, double _y);
		
        QtTerminal* getQtTerminal();

        void updateView(QtNode *node);

		protected:
		
                QtTerminal *m_QtTerminal;
	
	};

} //namespace FD


#endif
