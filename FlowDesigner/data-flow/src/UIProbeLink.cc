#include "UIProbeLink.h"
#include "UITerminal.h"
#include "Network.h"
#include "BufferedNode.h"
#include <sstream>
#include "PosixMutexLocker.h"

namespace FD
{
	using namespace std;


	DECLARE_NODE(UIProbeLinkNode);

	void UIProbeLinkNode::registerIF(UIObserverIF* client)
	{
		if (client)
		{
			//LOCK THE OBSERVERS LIST, WILL BE UNLOCKED IN PosixMutexLocker DESTRUCTOR
			PosixMutexLocker locker(&m_mutex);
			m_observers.push_back(client);
		}
	}
	
	void UIProbeLinkNode::unregisterIF(UIObserverIF* client)
	{
		if (client)
		{	
			//LOCK THE OBSERVERS LIST, WILL BE UNLOCKED IN PosixMutexLocker DESTRUCTOR
			PosixMutexLocker locker(&m_mutex);
			m_observers.remove(client);
		}
			
	}

	UIProbeLinkNode::UIProbeLinkNode(std::string nodeName, ParameterSet params)
		: BufferedNode(nodeName, params)
	{
		m_inputID = addInput("INPUT");
		m_outputID = addOutput("OUTPUT");

		//Mutex init
		#ifndef WIN32
		pthread_mutex_init(&m_mutex,NULL);
		#endif
	}

	UIProbeLinkNode::~UIProbeLinkNode()
	{
		//Mutex destroy
		#ifndef WIN32
		pthread_mutex_destroy(&m_mutex);
		#endif
	}

	void UIProbeLinkNode::calculate(int output_id, int count, Buffer &out)
	{
		//GET INPUT
  		ObjectRef ReturnValue = getInput(m_inputID, count);
		
		//LOCK THE OBSERVERS LIST, WILL BE UNLOCKED IN PosixMutexLocker DESTRUCTOR
		PosixMutexLocker locker(&m_mutex);
		
		//NOTIFY OBSERVERS
  		for(list<UIObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			//CLONING THE OBJET IS SAFER, BUT WILL THROW AN EXCEPTION
			//IF NOT IMPLEMENTED RIGHT NOW.
			try 
			{
				(*iter)->notify(ReturnValue->clone(),count);
			}
			catch(BaseException *e)
			{
				e->print(std::cerr);
				delete e;
				try 
				{
					(*iter)->notify(ReturnValue,count);
				}
				catch (BaseException *e)
				{						
					e->print(std::cerr);
					throw e->add(new GeneralException("Not working properly",__FILE__,__LINE__));
				}
			}
		}

		//INPUT = OUTPUT
  		out[count] = ReturnValue;
	 }
	


	UIProbeLink::UIProbeLink(UITerminal *_from, UITerminal *_to, const char *points_str, int _id)
	 : UILink(_from, _to, points_str, _id)
	{
		//SOMETHING TO DO ?
	}


	UIProbeLink::~UIProbeLink()
	{
	}
	
	void UIProbeLink::saveXML(xmlNode *root, int newId)
	{
		xmlNodePtr tree;
		if (m_points.size()<=2)
		{
			tree = xmlNewChild(root, NULL, (xmlChar *)"ProbeLink", NULL);

		}
		else 
		{
			stringstream str;
			list<GUILinkPoint*>::iterator it = m_points.begin();
			while(it != m_points.end())
			{
				str << (*it)->x << " " << (*it)->y << " ";
				it++;
			}
			tree = xmlNewChild(root, NULL, (xmlChar *)"ProbeLink", (xmlChar*)str.str().c_str());
		}
		
		this->id = newId;
		char idBuf[10] = {0};
		sprintf(idBuf,"%d", this->id);
		xmlSetProp(tree, (xmlChar *)"id", (xmlChar *)idBuf);
		xmlSetProp(tree, (xmlChar *)"from", (xmlChar *)from->getNode()->getName().c_str());
		xmlSetProp(tree, (xmlChar *)"output", (xmlChar *)from->getName().c_str());
		xmlSetProp(tree, (xmlChar *)"to", (xmlChar *)to->getNode()->getName().c_str());
		xmlSetProp(tree, (xmlChar *)"input", (xmlChar *)to->getName().c_str());

	}

   	void UIProbeLink::build(Network *net)
	{
		static int probenumber = 0;

		//THIS WILL CREATE A "DUMMY" NODE THAT IS CONNECTED TO 
		//ON BOTH SIDES (INPUT-OUTPUT) TO REAL PROCESSING NODES
		//THE "PROBE" NODE WILL CALL NOTIFY IN ITS GETOUTPUT FUNCTION
		//TO NOTIFY ALL OBSERVERS...
		stringstream nodename;
	
		nodename << "UIProbeLink_"<<probenumber++;
	
		//CREATE NODE
		UIProbeLinkNode* probeNode = new UIProbeLinkNode(nodename.str(),ParameterSet());

		//ADD NODE TO NETWORK
		net->addNode(*probeNode);
			
		//CONNECT NODES PROPERLY
		if (!to || !from)
			throw new GeneralException("Link is not connected at both ends", __FILE__, __LINE__);
		
		if (!to->getNode() || !from->getNode())
			throw new GeneralException("Cannot find node associated with link", __FILE__, __LINE__);
		
		//ADD PROBE IN THE MIDDLE OF STANDARD NODES!!!
		net->connect(to->getNode()->getName(), to->getName(), 
			nodename.str(),"OUTPUT");		

		net->connect(nodename.str(), "INPUT", 
			from->getNode()->getName(), from->getName());
		
		//REGISTER THIS NODE, THIS WILL BE USEFUL TO GET 
		//REGISTER OBSERVERS LATER AT RUN TIME
		UIProbeLink::getProbeDictionary().insert(make_pair(this,probeNode));
	}	


	void UIProbeLink::registerIF(UIObserverIF* client)
	{
		if (client && UIProbeLink::getProbeDictionary().find(this) != UIProbeLink::getProbeDictionary().end())
		{
			UIProbeLink::getProbeDictionary()[this]->registerIF(client);
		}
	}
	
	void UIProbeLink::unregisterIF(UIObserverIF* client)
	{
		if (client && UIProbeLink::getProbeDictionary().find(this) != UIProbeLink::getProbeDictionary().end())
		{
			UIProbeLink::getProbeDictionary()[this]->unregisterIF(client);
		}
	}
	
	std::map<UIProbeLink*, UIProbeLinkNode*> & UIProbeLink::getProbeDictionary()
	{
		static std::map<UIProbeLink*, UIProbeLinkNode*> probeMap;
		return probeMap;
	}	

} //namespace FD



