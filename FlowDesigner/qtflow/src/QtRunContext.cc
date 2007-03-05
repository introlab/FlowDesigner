#include "QtRunContext.h"
#include "Exception.h"
#include <sstream>
#include "flow_pref.h"
#include <unistd.h>
#include "UserException.h"

namespace FD 
{

	using namespace std;

	QtRunContext::QtRunContext(UIDocument *doc, ParameterSet &params)
		: m_document(doc), m_parameters(params), m_network(NULL)
	{

		//Create GUI
		

		

	}


	bool QtRunContext::run()
	{	
		bool success = false;
	
		if (m_document)
		{
			//build document
			try 
			{
				m_network = m_document->build("MAIN", m_parameters);
				
				//MAIN SHOULD NOT HAVE INPUT NODES
				if (m_network->getInputNode())
				{
					throw new GeneralException ("QtRunContext::run() - main network has input node", __FILE__, __LINE__);
				}
				
				//processing buffer requests
				for (int i = 0; ;i++) 
				{
					if (!m_network->hasOutput(i)) break;
					
					//empty request 
					ParameterSet req;
					m_network->request(i,req);
				}
				
				//initializing
				m_network->initialize();
				
				
				//Getting all outputs
				for (int i = 0; ;i++) 
				{
					if (!m_network->hasOutput(i)) break;
					
					if (1 /* FlowPref::getBool("VFLOW", "PrintOutput") */)
					{
						stringstream execOut;
						execOut << *m_network->getOutput(i,0);
						//PRINT IN GUI
						cout<<execOut.str()<<endl;
					} 
					else 
					{
						m_network->getOutput(i,0);
					}
				}
				
				//cerr << "Deleting in run()" << endl;
				//gdk_threads_enter();
				//less_print("Network ended normally");
				cout << "Network ended normally" << endl;
				success = true;
				//gdk_threads_leave();
	    
		
				
			} 
			catch (BaseException *e)
			{
				cerr << "Base Exception" << endl;
				stringstream excOut;
      
				e->print (excOut);
				//gdk_threads_enter();
				//less_print(excOut.str());
				cerr << excOut.str() <<endl;
				//gdk_threads_leave();
      
				delete e;				
				success = false;
			} 
			catch (UserException *e) 
			{
				cerr << "User stop caught" << endl;
				delete e;
				success = false;
			} 
			catch (...)
			{
				//gdk_threads_enter();
				//less_print("Unknown exception caught");
				//gdk_threads_leave();
				cerr << "Unknown exception caught "<<endl;
				success = false;
			}

			//DELETE RUNNING NETWORK
			if (m_network)
			{
				m_network->cleanupNotify();
				delete m_network;
				m_network=NULL;
			}
			
		} //if m_document
		
		return success;
	}

}//namespace FD
