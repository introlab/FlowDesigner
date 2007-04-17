
#include "UIDocumentController.h"
#include "UINetworkController.h"
#include <iostream>
#include "QtNetwork.h"

namespace FD
{

	using namespace std;

	UIDocumentController::UIDocumentController()
		: UIDocument("Untitled")
	{
	
	}

	UIDocumentController::UIDocumentController(const std::string &name, QtDocument *doc)
		: UIDocument(name), m_QtDocument(doc)
	{
	
	
	}
	
	
	UINetwork * UIDocumentController::newNetwork(const std::string &_name, UINetwork::Type type)
	{
		cerr<<"UINetwork * UIDocumentController::newNetwork(const std::string &_name, UINetwork::Type type)"<<endl;
		
		
		if (m_QtDocument)
		{
			//CREATING THE MODEL
			UINetworkController* net = new UINetworkController(this,_name,type);							
		
			//UPDATING THE VIEW
			m_QtDocument->addNetwork(net);
		
			return net;
		}
		else
		{
			return NULL;
		}
	
	}
	
	
}
