#ifndef _UINODECONTROLLER_H_
#define _UINODECONTROLLER_H_

#include "UINode.h"
#include <QObject>

namespace FD
{

	class UINetworkController;
	class QtNode;

	class UINodeController : public QObject, public UINode
	{
		Q_OBJECT;
		
		public:
		
		
			UINodeController(UINetworkController* _net, std::string _name, std::string _type, double x, double y);
		
			virtual UILink *newLink (UITerminal *_from, UITerminal *_to);

			virtual UITerminal* newTerminal(ItemInfo *_info, UINode *_node, bool _isInput, double _x, double _y);
			
			virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name,
					  const std::string &_objType="any", const std::string &_description="No description available");

			virtual UINodeParameters *newNodeParameters (UINode *_node, std::string type);
			
			virtual void rename (const std::string &newName);
		
			void setQtNode(QtNode *node){m_QtNode = node;}
			
			QtNode* getQtNode(){return m_QtNode;}
		
		private:
		
			UINodeController();
		
			QtNode* m_QtNode;
	
	};


} //namespace FD


#endif
