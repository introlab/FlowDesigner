#include <gtest/gtest.h>
#include "UINode.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include "UITerminal.h"
#include "UILink.h"
#include "UINote.h"
#include <vector>

// To use a test fixture, derive a class from testing::Test.
template <class T>
class  GenericUIFixture : public testing::Test, public virtual  FD::UIDocument::UIDocumentObserverIF, public virtual FD::UINetwork::UINetworkObserverIF
{

	// You should make the members protected s.t. they can be
	// accessed from sub-classes.
	protected:

	class UINodeEventReceiver :  public FD::UINode::UINodeObserverIF
	{
		public:

		//Those vectors will record every event received
		std::vector<std::pair<const FD::UINode*,const FD::UITerminal*> > m_notifyTerminalRemovedVector;
		std::vector<std::pair<const FD::UINode*,const FD::UITerminal*> > m_notifyTerminalAddedVector;
		std::vector<std::pair<const FD::UINode*,const FD::UINodeParameters*> > m_notifyParametersChangedVector;
		std::vector<const FD::UINode*> m_notifyDestroyedVector;
		std::vector<std::pair<const FD::UINode*,std::pair<double,double> > > m_notifyPositionChangedVector;
		std::vector<std::pair<const FD::UINode*,std::string> > m_notifyNameChangedVector;
		std::vector<std::pair<const FD::UINode*,std::string> > m_notifyTypeChangedVector;
		
		FD::UINode* m_node;
		
		UINodeEventReceiver(FD::UINode *node)
		: m_node(node)
		{
			if (m_node)
			{
				m_node->registerEvents(this);
			}
		}
		
		~UINodeEventReceiver()
		{
			if (m_node)
			{
				m_node->unregisterEvents(this);
			}
		}
		
		virtual void notifyTerminalRemoved(const FD::UINode *node, const FD::UITerminal* terminal)
		{
			m_notifyTerminalRemovedVector.push_back(std::make_pair(node,terminal));
		}

		virtual void notifyTerminalAdded(const FD::UINode *node, const FD::UITerminal* terminal)
		{
			m_notifyTerminalAddedVector.push_back(std::make_pair(node,terminal));
		}

		virtual void notifyParametersChanged(const FD::UINode *node, const FD::UINodeParameters *params)
		{
			m_notifyParametersChangedVector.push_back(std::make_pair(node,params));
		}

		virtual void notifyDestroyed(const FD::UINode *node)
		{
			m_notifyDestroyedVector.push_back(node);
			
			if (node == m_node)
			{
				m_node = NULL;
			}
		}

		virtual void notifyPositionChanged(const FD::UINode* node, double x, double y)
		{
			m_notifyPositionChangedVector.push_back(std::make_pair(node,std::make_pair(x,y)));
		}

		virtual void notifyNameChanged(const FD::UINode* node, const std::string &name)
		{
			m_notifyNameChangedVector.push_back(std::make_pair(node,name));
		}
		
		virtual void notifyTypeChanged(const FD::UINode* node, const std::string &type)
		{
			m_notifyTypeChangedVector.push_back(std::make_pair(node,type));
		}
		
	};

	class UINetworkEventReceiver : public FD::UINetwork::UINetworkObserverIF
	{
		public:
		
		FD::UINetwork *m_network;

		//Those vectors will record every event received
		std::vector<std::pair<const FD::UINetwork*,const FD::UINode*> > m_notifyNodeRemovedVector;
		std::vector<std::pair<const FD::UINetwork*,const FD::UINode*> > m_notifyNodeAddedVector;
		std::vector<std::pair<const FD::UINetwork*,const FD::UILink*> > m_notifyLinkRemovedVector;
		std::vector<std::pair<const FD::UINetwork*,const FD::UILink*> > m_notifyLinkAddedVector;
		std::vector<std::pair<const FD::UINetwork*,const FD::UINote*> > m_notifyNoteRemovedVector;
		std::vector<std::pair<const FD::UINetwork*,const FD::UINote*> > m_notifyNoteAddedVector;
		std::vector<std::pair<const FD::UINetwork*,const FD::UINetTerminal*> > m_notifyNetTerminalRemovedVector;
		std::vector<std::pair<const FD::UINetwork*,const FD::UINetTerminal*> > m_notifyNetTerminalAddedVector;
		std::vector<std::pair<const FD::UINetwork*,std::string> > m_notifyNameChangedVector;
		std::vector<std::pair<const FD::UINetwork*,std::string> > m_notifyDescriptionChangedVector;
		std::vector<const FD::UINetwork*> m_notifyDestroyedVector;
		
		UINetworkEventReceiver(FD::UINetwork *network)
		: m_network(network)
		{
			if (m_network)
			{
				m_network->registerEvents(this);
			}
		}

		~UINetworkEventReceiver()
		{
			if (m_network)
			{
				m_network->unregisterEvents(this);
			}
		}
		
		//Node removed
		virtual void notifyNodeRemoved(const FD::UINetwork *net, const FD::UINode* node)
		{
			m_notifyNodeRemovedVector.push_back(std::make_pair(net,node));
		}

		//Node added
		virtual void notifyNodeAdded(const FD::UINetwork *net, const FD::UINode* node)
		{
			m_notifyNodeAddedVector.push_back(std::make_pair(net,node));
		}

		//Link removed
		virtual void notifyLinkRemoved(const FD::UINetwork *net, const FD::UILink* link)
		{
			m_notifyLinkRemovedVector.push_back(std::make_pair(net,link));
		}

		//Link added
		virtual void notifyLinkAdded(const FD::UINetwork *net, const FD::UILink* link)
		{
			m_notifyLinkAddedVector.push_back(std::make_pair(net,link));
		}

		//Note removed
		virtual void notifyNoteRemoved(const FD::UINetwork *net, const FD::UINote* note)
		{
			m_notifyNoteRemovedVector.push_back(std::make_pair(net,note));
		}

		//Note added
		virtual void notifyNoteAdded(const FD::UINetwork *net, const FD::UINote* note)
		{
			m_notifyNoteAddedVector.push_back(std::make_pair(net,note));
		}

		//NetTerminal removed
		virtual void notifyNetTerminalRemoved(const FD::UINetwork *net, const FD::UINetTerminal* terminal)
		{
			m_notifyNetTerminalRemovedVector.push_back(std::make_pair(net,terminal));
		}

		//NetTerminal added
		virtual void notifyNetTerminalAdded(const FD::UINetwork *net, const FD::UINetTerminal* terminal)
		{
			m_notifyNetTerminalAddedVector.push_back(std::make_pair(net,terminal));
		}

		//Name changed
		virtual void notifyNameChanged(const FD::UINetwork *net, const std::string &oldName, const std::string &newName)
		{
			m_notifyNameChangedVector.push_back(std::make_pair(net,newName));
		}

		//Description changed
		virtual void notifyDescriptionChanged(const FD::UINetwork *net, const std::string &description)
		{
			m_notifyDescriptionChangedVector.push_back(std::make_pair(net,description));
		}

		//Destroyed
		virtual void notifyDestroyed(const FD::UINetwork *net)
		{
			m_notifyDestroyedVector.push_back(net);
			
			if (net == m_network)
			{
				m_network = NULL;
			}
		}
	};

	class UIDocumentEventReceiver : public FD::UIDocument::UIDocumentObserverIF
	{		
		public:
		
		FD::UIDocument *m_doc;
		
		//Those vectors will record every event received
		std::vector<std::pair<const FD::UIDocument*,const FD::UINetwork*> > m_notifyNetworkRemovedVector;
		std::vector<std::pair<const FD::UIDocument*,const FD::UINetwork*> > m_notifyNetworkAddedVector;
		std::vector<std::pair<const FD::UIDocument*,const FD::ItemInfo*> > m_notifyParametersChangedVector;
		std::vector<std::pair<const FD::UIDocument*,std::string> > m_notifyNameChangedVector;
		std::vector<std::pair<const FD::UIDocument*,std::string> > m_notifyPathChangedVector;
		std::vector<std::pair<const FD::UIDocument*,std::string> > m_notifyCategoryChangedVector;
		std::vector<std::pair<const FD::UIDocument*,std::string> > m_notifyCommentsChangedVector;
		std::vector<const FD::UIDocument*> m_notifyDestroyedVector;

		UIDocumentEventReceiver(FD::UIDocument* doc)
		:	m_doc(doc)
		{
			if (m_doc)
			{	
				m_doc->registerEvents(this);
			}
		}
		
		~UIDocumentEventReceiver()
		{
			if (m_doc)
			{
				m_doc->unregisterEvents(this);
			}
		}
		
		//Network removed
		virtual void notifyNetworkRemoved(const FD::UIDocument *doc, const FD::UINetwork* net) 
		{
			m_notifyNetworkRemovedVector.push_back(std::make_pair(doc,net));
		}
		
		//Network Added
		virtual void notifyNetworkAdded(const FD::UIDocument *doc, const FD::UINetwork* net) 
		{
			m_notifyNetworkAddedVector.push_back(std::make_pair(doc,net));
		}
				
		//Parameters changed
		virtual void notifyParametersChanged(const FD::UIDocument *doc, const FD::ItemInfo *param) 
		{
			m_notifyParametersChangedVector.push_back(std::make_pair(doc,param));
		}
		
		//Name changed
		virtual void notifyNameChanged(const FD::UIDocument *doc, const std::string &name)
		{
			m_notifyNameChangedVector.push_back(std::make_pair(doc,name));
		}
				
		//Path changed
		virtual void notifyPathChanged(const FD::UIDocument *doc, const std::string path)
		{
			m_notifyPathChangedVector.push_back(std::make_pair(doc,path));
		}
		
		//Category changed
		virtual void notifyCategoryChanged(const FD::UIDocument *doc, const std::string &category)
		{
			m_notifyCategoryChangedVector.push_back(std::make_pair(doc,category));
		}
		
		//Comments changed
		virtual void notifyCommentsChanged(const FD::UIDocument *doc, const std::string &comments)
		{
			m_notifyCommentsChangedVector.push_back(std::make_pair(doc,comments));
		}
		
		//Destroyed
		virtual void notifyDestroyed(const FD::UIDocument *doc) 
		{
			m_notifyDestroyedVector.push_back(doc);
			
			if (m_doc == doc)
			{	
				m_doc = NULL;
			}	
		}
	};

    FD::UIDocument *m_UIDocument;
    FD::UINetwork  *m_UINetwork;


	virtual void notifyDestroyed(const FD::UINetwork *net)
	{
		m_UINetwork = NULL;
	}

	virtual void notifyDestroyed(const FD::UIDocument *doc)
	{
		m_UIDocument = NULL;
	}

	// virtual void SetUp() will be called before each test is run.  You
	// should define it if you need to initialize the varaibles.
    // Otherwise, this can be skipped.
	virtual void SetUp()
	{
		m_UIDocument = new  FD::UIDocument("untitled");
		ASSERT_NE(m_UIDocument,(FD::UIDocument*)(NULL));
		m_UIDocument->registerEvents(this);
		m_UINetwork = m_UIDocument->addNetwork("MAIN",FD::UINetwork::subnet);
		ASSERT_NE(m_UINetwork,(FD::UINetwork*)(NULL));
		m_UINetwork->registerEvents(this);
	}

    // virtual void TearDown() will be called after each test is run.
    // You should define it if there is cleanup work to do.  Otherwise,
    // you don't have to provide it.
	virtual void TearDown()
    {
		if (m_UIDocument)
		{				
			delete m_UIDocument;
		}
	}


};
