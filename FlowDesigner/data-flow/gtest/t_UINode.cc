#include <limits.h>
#include <gtest/gtest.h>
#include "UINode.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include "UITerminal.h"
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

TEST_F(TestUINode,ALLOC_TEST)
{
	TestUINode::EventReceiver eventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	ASSERT_NE(node,(FD::UINode*)(NULL));

	EXPECT_EQ(node->getName(),"TESTNODE");
	EXPECT_EQ(node->getType(),"Constant");
	EXPECT_EQ(node->getNetwork(),m_UINetwork);

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

TEST_F(TestUINode,MOVE_TEST)
{
	TestUINode::EventReceiver eventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	node->registerEvents(&eventReceiver);

	//MOVE THE NODE
	node->setPos(10,10);

	//VERIFY IF WE RECEIVED THE DESTROYED EVENT
	EXPECT_EQ(eventReceiver.m_notifyPositionChangedCount,1);

	delete node;
}

TEST_F(TestUINode,RENAME_TEST)
{
	TestUINode::EventReceiver eventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	node->registerEvents(&eventReceiver);
	EXPECT_EQ(node->getName(),"TESTNODE");

	//TEST RENAME
	node->rename("TESTNODE2");
	EXPECT_EQ(node->getName(),"TESTNODE2");

	//TEST RENAME EVENT
	EXPECT_EQ(eventReceiver.m_notifyNameChangedCount,1);
	delete node;

}

TEST_F(TestUINode,ADD_TERMINAL_TEST)
{
	TestUINode::EventReceiver eventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	node->registerEvents(&eventReceiver);

	node->addTerminal("TERM1",FD::UINetTerminal::INPUT,"any","No description available");

	//TEST VALUES
	FD::UITerminal *term = node->getInputNamed("TERM1");
	EXPECT_NE(term,(FD::UITerminal*)(NULL));
	EXPECT_EQ(term->getNode(),node);
	EXPECT_EQ(term->getName(),"TERM1");
	EXPECT_EQ(term->getType(),"any");
	EXPECT_EQ(term->getDescription(),"No description available");

	//TEST ADD EVENT
	EXPECT_EQ(eventReceiver.m_notifyTerminalAddedCount,1);

	delete node;
}

TEST_F(TestUINode,REMOVE_TERMINAL_TEST)
{
	TestUINode::EventReceiver eventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	node->registerEvents(&eventReceiver);

	node->addTerminal("TERM1",FD::UINetTerminal::INPUT,"any","No description available");

	//REMOVING NON EXISTING
	node->removeTerminal("TERM2",FD::UINetTerminal::INPUT);
	EXPECT_EQ(eventReceiver.m_notifyTerminalRemovedCount,0);

	//REMOVING WRONG TYPE
	node->removeTerminal("TERM1",FD::UINetTerminal::OUTPUT);
	EXPECT_EQ(eventReceiver.m_notifyTerminalRemovedCount,0);

	//REMOVING RIGHT TYPE
	node->removeTerminal("TERM1",FD::UINetTerminal::INPUT);
	EXPECT_EQ(eventReceiver.m_notifyTerminalRemovedCount,1);

	delete node;

}
