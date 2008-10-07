#include <gtest/gtest.h>
#include "UINode.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include "UITerminal.h"


// To use a test fixture, derive a class from testing::Test.
template <class T>
class  GenericUIFixture : public testing::Test
{

	// You should make the members protected s.t. they can be
	// accessed from sub-classes.
	protected:

	class EventReceiver :  public FD::UINode::UINodeObserverIF
	{
		public:

		EventReceiver()
		{
			m_notifyChangedCount = 0;
			m_notifyTerminalRemovedCount = 0;
			m_notifyTerminalAddedCount = 0;
			m_notifyParametersChangedCount = 0;
			m_notifyDestroyedCount = 0;
			m_notifyPositionChangedCount = 0;
			m_notifyNameChangedCount = 0;
		}

		//Global event for changes
		virtual void notifyChanged(const FD::UINode* node)
		{
			m_notifyChangedCount++;
		}

		virtual void notifyTerminalRemoved(const FD::UINode *node, const FD::UITerminal* terminal)
		{
			m_notifyTerminalRemovedCount++;
		}

		virtual void notifyTerminalAdded(const FD::UINode *node, const FD::UITerminal* terminal)
		{
			m_notifyTerminalAddedCount++;
		}

		virtual void notifyParametersChanged(const FD::UINode *node, const FD::UINodeParameters *params)
		{
			m_notifyParametersChangedCount++;
		}

		virtual void notifyDestroyed(const FD::UINode *node)
		{
			m_notifyDestroyedCount++;
		}

		virtual void notifyPositionChanged(const FD::UINode* node, double x, double y)
		{
			m_notifyPositionChangedCount++;
		}

		virtual void notifyNameChanged(const FD::UINode* node, const std::string &name)
		{
			m_notifyNameChangedCount++;
		}

		int m_notifyChangedCount;
		int m_notifyTerminalRemovedCount;
		int m_notifyTerminalAddedCount;
		int m_notifyParametersChangedCount;
		int m_notifyDestroyedCount;
		int m_notifyPositionChangedCount;
		int m_notifyNameChangedCount;

	};


    FD::UIDocument *m_UIDocument;
    FD::UINetwork  *m_UINetwork;


	// virtual void SetUp() will be called before each test is run.  You
	// should define it if you need to initialize the varaibles.
    // Otherwise, this can be skipped.
	virtual void SetUp()
	{
		m_UIDocument = new  FD::UIDocument("untitled");
		ASSERT_NE(m_UIDocument,(FD::UIDocument*)(NULL));
		m_UINetwork = m_UIDocument->newNetwork("MAIN",FD::UINetwork::subnet);
		ASSERT_NE(m_UINetwork,(FD::UINetwork*)(NULL));
	}

    // virtual void TearDown() will be called after each test is run.
    // You should define it if there is cleanup work to do.  Otherwise,
    // you don't have to provide it.
	virtual void TearDown()
    {
		if (m_UIDocument)
			delete m_UIDocument;
    }



};
