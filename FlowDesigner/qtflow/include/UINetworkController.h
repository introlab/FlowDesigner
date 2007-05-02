#ifndef _UINETWORKCONTROLLER_H_
#define _UINETWORKCONTROLLER_H_

#include "UINetwork.h"
#include <QObject>

namespace FD
{

	class UIDocumentController;
	class UINodeController;
	class QtNetwork;
    class QtDocument;

	class UINetworkController : public QObject, public UINetwork
	{
		Q_OBJECT;
		
		public:
		
		UINetworkController(UIDocumentController* doc, const std::string &_name, UINetwork::Type type);

        /**Construct a UINetwork from a parsed XML file*/
        UINetworkController(UIDocumentController* _doc, xmlNodePtr net);

		
		UINodeController* createNode(std::string type, double x, double y, bool doInit);
		
		//must be re-implemented from UINetwork
		virtual UINode *newNode(UINetwork* _net, std::string _name, 
						   std::string _type, double _x, double _y, bool doInit);


                virtual UINode *newNode(UINetwork* _net, xmlNodePtr def);

						   
		virtual UILink *newLink (UITerminal *_from, UITerminal *_to,const char *str=NULL);
		
		virtual UINote* newNote(const std::string &text, double x, double y, bool visible);

		virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name, 
					  const std::string &_objType="any", const std::string &_description="No description available");
		
	
		void setQtNetwork(QtNetwork *network) {m_QtNetwork = network;}
		
		QtNetwork* getQtNetwork(){return m_QtNetwork;}
	
        void updateView(QtDocument *doc);

		private:
		
		UINetworkController();
		
		QtNetwork *m_QtNetwork;
	
	};

} //namespace FD

#endif
