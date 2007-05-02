
#include "UIDocumentController.h"
#include "UINetworkController.h"
#include <iostream>
#include "QtNetwork.h"

namespace FD
{

	using namespace std;

	UIDocumentController::UIDocumentController(const std::string &name, QtDocument *doc)
		: UIDocument(name), m_QtDocument(doc)
	{
	
            cerr<<"UIDocumentController::UIDocumentController";
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
            cerr<<"WARNING UIDocumentController::newNetwork ---> no QtDocument";
			return new UINetworkController(this,_name,type);
		}
	
	}
	
	UINetwork* UIDocumentController::newNetwork(xmlNodePtr _net)
    {

        cerr<<"UINetwork* UIDocumentController::newNetwork (XML)"<<endl;

		if (m_QtDocument)
		{
			//CREATING THE MODEL
			UINetworkController* net = new UINetworkController(this,_net);							
		
	        //UPDATING THE VIEW
			//m_QtDocument->addNetwork(net);
		
			return net;
		}
		else
		{
            cerr<<"WARNING UIDocumentController::newNetwork ---> no QtDocument";
			return new UINetworkController(this,_net);
		}

    }

    void UIDocumentController::updateView()
    {
        if (m_QtDocument)
        {
            //CALL UPDATE VIEW ON EACH UINETWORKCONTROLLER
            for (unsigned int i = 0; i < networks.size(); i++)
            {
                UINetworkController *ctrl = dynamic_cast<UINetworkController*>(networks[i]);

                if (ctrl)
                {
                    ctrl->updateView(m_QtDocument);
                }

            }
            
        }
        else
        {
            cerr<<"ERROR : UIDocumentController::updateView() no QtDocument defined"<<endl;
        }

    }
}
