#ifndef _UILINKCONTROLLER_H_
#define _UILINKCONTROLLER_H_

#include "UILink.h"
#include <QObject>


namespace FD
{

    class UITerminalController;
    class QtNetwork;
    class QtLink;

	class UILinkController : public QObject, public UILink
	{
		Q_OBJECT;
		
		public:
		
		UILinkController(UITerminalController *_from, 
                         UITerminalController *_to, 
                         const char *points_str=NULL);

		
        void updateView(QtNetwork *net);
		
        QtLink* getQtLink(){return m_QtLink;}
		
		protected:
    
        QtLink *m_QtLink;
	
	};


} //namespace FD


#endif