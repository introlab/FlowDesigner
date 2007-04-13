#ifndef _UINETWORKCONTROLLER_H_
#define _UINETWORKCONTROLLER_H_

#include "UINetwork.h"
#include <QObject>

namespace FD
{

	class UIDocumentController;

	class UINetworkController : public QObject, public UINetwork
	{
		Q_OBJECT;
		
		public:
		
		//must be re-implemented from UINetwork
		virtual UINode *newNode(UINetwork* _net, std::string _name, 
						   std::string _type, double _x, double _y, bool doInit);
						   
		virtual UILink *newLink (UITerminal *_from, UITerminal *_to,const char *str=NULL);
		
		virtual UINote* newNote(const std::string &text, double x, double y, bool visible);

		virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name, 
					  const std::string &_objType="any", const std::string &_description="No description available");
		
	
		private:
		
		UINetworkController();
	
	};

} //namespace FD

#endif
