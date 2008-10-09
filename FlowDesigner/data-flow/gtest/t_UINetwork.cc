#include "t_GenericUIFixture.h"

typedef GenericUIFixture<FD::UINetwork> TestUINetwork;

TEST_F(TestUINetwork,ADD_REMOVE_NODE_TEST)
{
	UINetworkEventReceiver eventReceiver(m_UINetwork);
	
	FD::UINode *node = new FD::UINode(m_UINetwork,"NODE","Constant",0,0,true);
	EXPECT_EQ(node,m_UINetwork->getNodeNamed("NODE"));
	EXPECT_EQ(eventReceiver.m_notifyNodeAddedVector.size(),1);
	
	//DUPLICATED ADD
	EXPECT_FALSE(m_UINetwork->addNode(node));
	EXPECT_EQ(eventReceiver.m_notifyNodeAddedVector.size(),1);
	
	//DELETE NODE
	delete node;
	
	EXPECT_EQ((FD::UINode*)NULL,m_UINetwork->getNodeNamed("NODE"));
	EXPECT_EQ(eventReceiver.m_notifyNodeRemovedVector.size(),1);
	
	
	//DUPLICATED REMOVE
	EXPECT_FALSE(m_UINetwork->removeNode(node));
	
}

TEST_F(TestUINetwork,ADD_REMOVE_NOTE_TEST)
{

}


TEST_F(TestUINetwork,ADD_REMOVE_NET_TERMINAL)
{
	
}