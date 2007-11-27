#ifndef _UILINKCONTROLLER_H_
#define _UILINKCONTROLLER_H_

#include "UIProbeLink.h"
#include <QObject>


namespace FD
{
    
	class UITerminalController;
	class QtNetwork;
	class QtLink;
    
	class UILinkController : public QObject, public UIProbeLink
	{
        Q_OBJECT;
        
		public:
        
        UILinkController ( UITerminalController *_from,
        UITerminalController *_to,
        const char *points_str=NULL );
        
        QtLink* getQtLink() {return m_QtLink;}
        void setQtLink(QtLink *link) {m_QtLink = link;}
        
		public slots:
        void positionChanged ( double x, double y );
        
        void updateView ( QtNetwork *net );
        
        bool valid();
        
		protected:
        
        QtLink *m_QtLink;
        
	};
    
    
} //namespace FD


#endif
