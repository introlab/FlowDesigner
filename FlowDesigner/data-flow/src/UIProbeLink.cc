#include "UIProbeLink.h"
#include "UITerminal.h"
#include "Network.h"
#include "BufferedNode.h"
#include <sstream>

namespace FD
{
	using namespace std;


	DECLARE_NODE(UIProbeLinkNode);

	void UIProbeLinkNode::registerIF(UIObserverIF* client)
	{
		if (client)
		{
			m_observers.push_back(client);
		}
	}
	
	void UIProbeLinkNode::unregisterIF(UIObserverIF* client)
	{
		if (client)
		{
			m_observers.remove(client);
		}
			
	}

	UIProbeLinkNode::UIProbeLinkNode(std::string nodeName, ParameterSet params)
		: BufferedNode(nodeName, params)
	{
		m_inputID = addInput("INPUT");
		m_outputID = addOutput("OUTPUT");
	}

	void UIProbeLinkNode::calculate(int output_id, int count, Buffer &out)
	{
		//GET INPUT
  		ObjectRef ReturnValue = getInput(m_inputID, count);
		
		//NOTIFY OBSERVERS
  		for(list<UIObserverIF*>::iterator iter = m_observers.begin(); iter != m_observers.end(); iter++)
		{
			//CLONING THE OBJET IS SAFER, BUT WILL THROW AN EXCEPTION
			//IF NOT IMPLEMENTED RIGHT NOW.
			try 
			{
				std::cerr<<"UIProbeLinkNode::calculate -- will notify"<<std::endl;
				(*iter)->notify(ReturnValue->clone(),count);
				std::cerr<<"UIProbeLinkNode::calculate --notify complete"<<std::endl;
			}
			catch(BaseException *e)
			{
				e->print(std::cerr);
				delete e;
				try 
				{
					(*iter)->notify(ReturnValue,count);

					std::cerr<<"UIProbeLinkNode::calculate --notify complete"<<std::endl;
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
	


	UIProbeLink::UIProbeLink(UITerminal *_from, UITerminal *_to, const char *points_str)
	 : UILink(_from, _to, points_str)
	{
		//SOMETHING TO DO ?
	}


	UIProbeLink::~UIProbeLink()
	{
		//SOMETHING TO DO ?
	}
	
	void UIProbeLink::saveXML(xmlNode *root)
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



