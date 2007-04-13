#ifndef _UINODECONTROLLER_H_
#define _UINODECONTROLLER_H_

#include "UINode.h"
#include <QObject>

namespace FD
{

	class UINodeController : public QObject, public UINode
	{
		Q_OBJECT;
		
		public:
		
			virtual UILink *newLink (UITerminal *_from, UITerminal *_to);

			virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name,
					  const std::string &_objType="any", const std::string &_description="No description available");

			virtual UINodeParameters *newNodeParameters (UINode *_node, std::string type);
			
			virtual void rename (const std::string &newName);
		
		private:
		
		UINodeController();
	
	};


} //namespace FD


#endif
