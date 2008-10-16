

#include "t_GenericUIFixture.h"

typedef GenericUIFixture<FD::UINetwork> TestUINetwork;

TEST_F(TestUINetwork,ADD_REMOVE_NODE_TEST)
{
	UINetworkEventReceiver eventReceiver(m_UINetwork);
	
	FD::UINode *node = new FD::UINode(m_UINetwork,"NODE","Constant",0,0);
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
	UINetworkEventReceiver eventReceiver(m_UINetwork);
	
	//CREATE NODE
	FD::UINode *node = new FD::UINode(m_UINetwork,"NODE","Constant",0,0);
	EXPECT_EQ(node,m_UINetwork->getNodeNamed("NODE"));
	EXPECT_EQ(eventReceiver.m_notifyNodeAddedVector.size(),1);
	
	//CREATE NET TERMINAL WITH NULL TERMINAL SHOULD NOT WORK
	FD::UINetTerminal *invalidTerm = m_UINetwork->createNetTerminal(NULL,FD::UINetTerminal::INPUT,"NAME","TYPE","DESCRIPTION");
	EXPECT_EQ(invalidTerm,(FD::UINetTerminal*)NULL);
	EXPECT_EQ(eventReceiver.m_notifyNetTerminalAddedVector.size(),0);
	EXPECT_EQ(eventReceiver.m_notifyNetTerminalRemovedVector.size(),0);
	
	//CREATE TERMINAL & NET TERMINAL
	FD::ItemInfo info("INPUT","TYPE","VALUE","DESCRIPTION");
	FD::UITerminal *term = node->createTerminal(&info,true,0,0);
	EXPECT_EQ(node->getInputNamed("INPUT"),term);
	
	//DUPLICATED ADD (UINODE)
	EXPECT_FALSE(node->addTerminal(term));
	FD::UINetTerminal *netTerm = m_UINetwork->createNetTerminal(term,FD::UINetTerminal::INPUT,"NET_INPUT","TYPE","DESCRIPTION");
	EXPECT_EQ(netTerm->getTerminal(),term);
	EXPECT_EQ(term->getNetTerminal(),netTerm);
	
	//DUPLICATED ADD (UINETWORK)
	EXPECT_FALSE(m_UINetwork->addNetTerminal(netTerm));
	EXPECT_TRUE(m_UINetwork->haveNetTerminal(netTerm));
	EXPECT_EQ(eventReceiver.m_notifyNetTerminalAddedVector.size(),1);
	
	//DELETE NET TERMINAL
	delete netTerm;
	EXPECT_EQ(eventReceiver.m_notifyNetTerminalRemovedVector.size(),1);
	EXPECT_NE(netTerm,term->getNetTerminal());
	EXPECT_EQ((FD::UINetTerminal*)NULL,term->getNetTerminal());
	EXPECT_FALSE(m_UINetwork->haveNetTerminal(netTerm));
	
	//DUPLICATED REMOVE
	EXPECT_FALSE(m_UINetwork->removeNetTerminal(netTerm));

	//CLEANING UP...
	delete node;
	
	
}