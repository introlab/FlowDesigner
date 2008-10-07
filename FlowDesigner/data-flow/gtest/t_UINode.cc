#include <iostream>
#include "t_GenericUIFixture.h"

typedef GenericUIFixture<FD::UINode> TestUINode;

TEST_F(TestUINode,ALLOC_TEST)
{
	TestUINode::UINodeEventReceiver UINodeEventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	ASSERT_NE(node,(FD::UINode*)(NULL));

	EXPECT_EQ(node->getName(),"TESTNODE");
	EXPECT_EQ(node->getType(),"Constant");
	EXPECT_EQ(node->getNetwork(),m_UINetwork);

	node->registerEvents(&UINodeEventReceiver);

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
	EXPECT_EQ(UINodeEventReceiver.m_notifyDestroyedVector.size(),1);

}

TEST_F(TestUINode,MOVE_TEST)
{
	TestUINode::UINodeEventReceiver UINodeEventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	node->registerEvents(&UINodeEventReceiver);

	//MOVE THE NODE
	node->setPos(10,10);

	//VERIFY IF WE RECEIVED THE DESTROYED EVENT
	EXPECT_EQ(UINodeEventReceiver.m_notifyPositionChangedVector.size(),1);

	delete node;
}

TEST_F(TestUINode,RENAME_TEST)
{
	TestUINode::UINodeEventReceiver UINodeEventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	node->registerEvents(&UINodeEventReceiver);
	EXPECT_EQ(node->getName(),"TESTNODE");

	//TEST RENAME
	node->rename("TESTNODE2");
	EXPECT_EQ(node->getName(),"TESTNODE2");

	//TEST RENAME EVENT
	EXPECT_EQ(UINodeEventReceiver.m_notifyNameChangedVector.size(),1);
	delete node;

}

TEST_F(TestUINode,ADD_TERMINAL_TEST)
{
	TestUINode::UINodeEventReceiver UINodeEventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	node->registerEvents(&UINodeEventReceiver);

	node->addTerminal("TERM1",FD::UINetTerminal::INPUT,"any","No description available");

	//TEST WRONG TYPE
	EXPECT_EQ(node->getOutputNamed("TERM1"),(FD::UITerminal*)(NULL));

	//TEST VALUES
	FD::UITerminal *term = node->getInputNamed("TERM1");
	EXPECT_NE(term,(FD::UITerminal*)(NULL));
	EXPECT_EQ(term->getNode(),node);
	EXPECT_EQ(term->getName(),"TERM1");
	EXPECT_EQ(term->getType(),"any");
	EXPECT_EQ(term->getDescription(),"No description available");

	//TEST ADD EVENT
	EXPECT_EQ(UINodeEventReceiver.m_notifyTerminalAddedVector.size(),1);

	delete node;
}

TEST_F(TestUINode,REMOVE_TERMINAL_TEST)
{
	TestUINode::UINodeEventReceiver UINodeEventReceiver;

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	node->registerEvents(&UINodeEventReceiver);

	node->addTerminal("TERM1",FD::UINetTerminal::INPUT,"any","No description available");

	//REMOVING NON EXISTING
	node->removeTerminal("TERM2",FD::UINetTerminal::INPUT);
	EXPECT_EQ(UINodeEventReceiver.m_notifyTerminalRemovedVector.size(),0);

	//REMOVING WRONG TYPE
	node->removeTerminal("TERM1",FD::UINetTerminal::OUTPUT);
	EXPECT_EQ(UINodeEventReceiver.m_notifyTerminalRemovedVector.size(),0);

	//REMOVING RIGHT TYPE
	node->removeTerminal("TERM1",FD::UINetTerminal::INPUT);
	EXPECT_EQ(UINodeEventReceiver.m_notifyTerminalRemovedVector.size(),1);

	delete node;

}

TEST_F(TestUINode,PARAMETERS_TEST)
{
	//TODO PARAMETERS TEST
}

