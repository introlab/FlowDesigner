#ifndef _UILINKCONTROLLER_H_
#define _UILINKCONTROLLER_H_

#include "UILink.h"
#include <QObject>

namespace FD
{
	class UILinkController : public QObject, public UILink
	{
		Q_OBJECT;
		
		public:
		
		
		private:
		
		UILinkController();
		
	
	};


} //namespace FD


#endif
