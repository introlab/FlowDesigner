//Copyright (C) 2007 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)
#include "QtRunContext.h"
#include "Exception.h"
#include <sstream>
#include "flow_pref.h"
#include <unistd.h>
#include "UserException.h"
#include "QtProbeManager.h"

//ALL RUNNING INFORMATION WILL BE HELD HERE!
namespace FD 
{

	using namespace std;

	QtRunContext::QtRunContext(UIDocument *doc, ParameterSet &params)
		: m_document(doc), m_parameters(params), m_network(NULL)
	{

		//THIS WILL CREATE A SOCKET SERVER
		//FOR PROBES
		m_probeManager = new QtProbeManager(this);

		

	}

	QtRunContext::~QtRunContext()
	{
		
		//STOP AND DELETE PROBE MANAGER
		
		
		
		//MEMORY CLEANUP
		if (m_document)
		{
			delete m_document;
		}
		
		
		
		
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
				
				/*
				std::vector<ItemInfo *> outputs = m_document->getNetOutputs("MAIN");
				
				for (unsigned int i = 0; i < outputs.size(); i++)
				{
					cout << "outputs name "<<outputs[i]->name << endl;
					cout << "outputs type "<<outputs[i]->type << endl;
					cout << "outputs value "<<outputs[i]->value<< endl;
					cout << "outputs description "<<outputs[i]->description<< endl;
					
				}
				*/
				
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

				cout << "Network ended normally" << endl;
				success = true;				
			} 
			catch (BaseException *e)
			{
				cerr << "Base Exception" << endl;
				e->print (cerr);
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
