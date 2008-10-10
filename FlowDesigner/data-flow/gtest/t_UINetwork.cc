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
	UINetworkEventReceiver eventReceiver(m_UINetwork);

	//CREATE NOTE
	FD::UINote *note = new FD::UINote(m_UINetwork,"Label","This is a note.", 0, 0 , true);	
	EXPECT_NE((FD::UINote*)NULL,note);
	EXPECT_EQ(eventReceiver.m_notifyNoteAddedVector.size(),1);
	
	//DUPLICATED ADD
	EXPECT_FALSE(m_UINetwork->addNote(note));
	
	//DELETE NOTE
	delete note;
	EXPECT_EQ(eventReceiver.m_notifyNoteRemovedVector.size(),1);
	
	//DUPLICATED REMOVE
	EXPECT_FALSE(m_UINetwork->removeNote(note));
	
}


TEST_F(TestUINetwork,ADD_REMOVE_NET_TERMINAL)
{
	
}