#include <limits.h>
#include <gtest/gtest.h>
#include "UINode.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include <iostream>

// To use a test fixture, derive a class from testing::Test.
class  TestUINode : public testing::Test
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

		int m_notifyChangedCount;
		int m_notifyTerminalRemovedCount;
		int m_notifyTerminalAddedCount;
		int m_notifyParametersChangedCount;
		int m_notifyDestroyedCount;
		int m_notifyPositionChangedCount;

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

TEST_F(TestUINode,ALLOC_TEST)
{
	TestUINode::EventReceiver eventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	ASSERT_NE(node,(FD::UINode*)(NULL));

	node->registerEvents(&eventReceiver);

	//ADD NODE TO THE NETWORK
	m_UINetwork->addNode(node);

	//VERIFY IF PROPERLY ADDED
	FD::UINode *n1 = m_UINetwork->getNodeNamed("TESTNODE");
	EXPECT_EQ(n1,node);

	delete node;

	//VERIFY IF PROPERLY REMOVED (THIS TEST WILL FAIL RIGHT NOW)
	FD::UINode *n2 = m_UINetwork->getNodeNamed("TESTNODE");
	EXPECT_EQ(n2,(FD::UINode*)(NULL));

	//VERIFY IF WE RECEIVED THE DESTROYED EVENT
	EXPECT_EQ(eventReceiver.m_notifyDestroyedCount,1);

}
