#include <iostream>
#include "t_GenericUIFixture.h"

typedef GenericUIFixture<FD::UINode> TestUINode;

TEST_F(TestUINode,ALLOC_TEST)
{
	
	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	ASSERT_NE(node,(FD::UINode*)(NULL));
	
	TestUINode::UINodeEventReceiver UINodeEventReceiver(node);

	EXPECT_EQ(node->getName(),"TESTNODE");
	EXPECT_EQ(node->getType(),"Constant");
	EXPECT_EQ(node->getNetwork(),m_UINetwork);

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
	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	TestUINode::UINodeEventReceiver UINodeEventReceiver(node);


	//MOVE THE NODE
	node->setPos(10,10);

	//VERIFY IF WE RECEIVED THE DESTROYED EVENT
	EXPECT_EQ(UINodeEventReceiver.m_notifyPositionChangedVector.size(),1);

	delete node;
}

TEST_F(TestUINode,RENAME_TEST)
{
	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	TestUINode::UINodeEventReceiver UINodeEventReceiver(node);

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
	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	TestUINode::UINodeEventReceiver UINodeEventReceiver(node);


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
	

	//CREATE NODE
	FD::UINode *node = m_UINetwork->newNode(m_UINetwork,"TESTNODE","Constant",0,0,true);
	TestUINode::UINodeEventReceiver UINodeEventReceiver(node);


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

TEST_F(TestUINode,LINK_TEST_TWO_NODES)
{

	//CREATE NODE 1
	FD::UINode *node1 = m_UINetwork->newNode(m_UINetwork,"NODE1","TYPE1",0,0,true);
	m_UINetwork->addNode(node1);
	EXPECT_EQ(node1,m_UINetwork->getNodeNamed("NODE1"));
	TestUINode::UINodeEventReceiver UINodeEventReceiver1(node1);


	//ADD OUTPUT TERMINAL TO NODE1
	FD::UITerminal* outputTerminal = node1->addTerminal("TERM1",FD::UINetTerminal::OUTPUT,"any","No description available");
	EXPECT_EQ(UINodeEventReceiver1.m_notifyTerminalAddedVector.size(),1);
	EXPECT_NE(outputTerminal,(FD::UITerminal*)NULL);
	EXPECT_EQ(outputTerminal,node1->getOutputNamed("TERM1"));
	EXPECT_EQ(outputTerminal->getNode(),node1);


	//CREATE NODE 2
	FD::UINode *node2 = m_UINetwork->newNode(m_UINetwork,"NODE2","TYPE2",0,0,true);
	m_UINetwork->addNode(node2);
	EXPECT_EQ(node2,m_UINetwork->getNodeNamed("NODE2"));
	TestUINode::UINodeEventReceiver UINodeEventReceiver2(node2);


	//ADD INPUT TERMINAL TO NODE2
	FD::UITerminal* inputTerminal = node2->addTerminal("TERM2",FD::UINetTerminal::INPUT,"any","No description available");
	EXPECT_EQ(UINodeEventReceiver2.m_notifyTerminalAddedVector.size(),1);
	EXPECT_NE(inputTerminal,(FD::UITerminal*)NULL);
	EXPECT_EQ(inputTerminal,node2->getInputNamed("TERM2"));
	EXPECT_EQ(inputTerminal->getNode(),node2);


	//LINK NODES
	FD::UILink *link = m_UINetwork->newLink(outputTerminal,inputTerminal);
	EXPECT_NE(link,(FD::UILink*)NULL);
	EXPECT_EQ(link->getFromTerminal(),outputTerminal);
	EXPECT_EQ(link->getToTerminal(),inputTerminal);

	//MAKE SURE NODES ARE AWARE OF THE NEW LINK
	EXPECT_EQ(true,inputTerminal->haveLink(link));
	EXPECT_EQ(true,outputTerminal->haveLink(link));

	//DESTROY LINK
	delete link;

	//MAKE SURE NODES ARE AWARE THAT THE LINK DOES NOT EXIST ANYMORE
	EXPECT_EQ(false,inputTerminal->haveLink(link));
	EXPECT_EQ(false,outputTerminal->haveLink(link));


	//DESTROY NODES
	delete node1;
	EXPECT_EQ((FD::UINode*)NULL,m_UINetwork->getNodeNamed("NODE1"));
	delete node2;
	EXPECT_EQ((FD::UINode*)NULL,m_UINetwork->getNodeNamed("NODE2"));
}

TEST_F(TestUINode,LINK_TEST_THREE_NODES)
{
	

	//CREATE NODE 1
	FD::UINode *node1 = m_UINetwork->newNode(m_UINetwork,"NODE1","TYPE1",0,0,true);
	m_UINetwork->addNode(node1);
	EXPECT_EQ(node1,m_UINetwork->getNodeNamed("NODE1"));
	TestUINode::UINodeEventReceiver UINodeEventReceiver1(node1);


	//ADD OUTPUT TERMINAL TO NODE1
	FD::UITerminal* outputTerminal_1 = node1->addTerminal("TERM1",FD::UINetTerminal::OUTPUT,"any","No description available");
	EXPECT_EQ(UINodeEventReceiver1.m_notifyTerminalAddedVector.size(),1);
	EXPECT_NE(outputTerminal_1,(FD::UITerminal*)NULL);
	EXPECT_EQ(outputTerminal_1,node1->getOutputNamed("TERM1"));
	EXPECT_EQ(outputTerminal_1->getNode(),node1);

	//CREATE NODE 2
	FD::UINode *node2 = m_UINetwork->newNode(m_UINetwork,"NODE2","TYPE2",0,0,true);
	m_UINetwork->addNode(node2);
	EXPECT_EQ(node2,m_UINetwork->getNodeNamed("NODE2"));
	TestUINode::UINodeEventReceiver UINodeEventReceiver2(node2);


	//ADD INPUT &  OUTPUT TERMINAL TO NODE 2
	FD::UITerminal* inputTerminal_2 = node2->addTerminal("TERM2",FD::UINetTerminal::INPUT,"any","No description available");
	EXPECT_EQ(UINodeEventReceiver2.m_notifyTerminalAddedVector.size(),1);
	EXPECT_NE(inputTerminal_2,(FD::UITerminal*)NULL);
	EXPECT_EQ(inputTerminal_2,node2->getInputNamed("TERM2"));
	EXPECT_EQ(inputTerminal_2->getNode(),node2);

	FD::UITerminal* outputTerminal_2 = node2->addTerminal("TERM3",FD::UINetTerminal::OUTPUT,"any","No description available");
	EXPECT_EQ(UINodeEventReceiver2.m_notifyTerminalAddedVector.size(),2);
	EXPECT_NE(outputTerminal_2,(FD::UITerminal*)NULL);
	EXPECT_EQ(outputTerminal_2,node2->getOutputNamed("TERM3"));
	EXPECT_EQ(outputTerminal_2->getNode(),node2);

	//CREATE NODE 3
	FD::UINode *node3 = m_UINetwork->newNode(m_UINetwork,"NODE3","TYPE3",0,0,true);
	m_UINetwork->addNode(node3);
	EXPECT_EQ(node3,m_UINetwork->getNodeNamed("NODE3"));
	TestUINode::UINodeEventReceiver UINodeEventReceiver3(node3);


	//ADD INPUT TERMINAL TO NODE 3
	FD::UITerminal* inputTerminal_3 = node3->addTerminal("TERM4",FD::UINetTerminal::INPUT,"any","No description available");
	EXPECT_EQ(UINodeEventReceiver3.m_notifyTerminalAddedVector.size(),1);
	EXPECT_NE(inputTerminal_3,(FD::UITerminal*)NULL);
	EXPECT_EQ(inputTerminal_3,node3->getInputNamed("TERM4"));
	EXPECT_EQ(inputTerminal_3->getNode(),node3);


	//LINK NODE 1 & NODE2
	FD::UILink *link1 = m_UINetwork->newLink(outputTerminal_1,inputTerminal_2);
	EXPECT_NE(link1,(FD::UILink*)NULL);
	EXPECT_EQ(link1->getFromTerminal(),outputTerminal_1);
	EXPECT_EQ(link1->getToTerminal(),inputTerminal_2);

	//MAKE SURE NODES ARE AWARE OF THE NEW LINK
	EXPECT_EQ(true,inputTerminal_2->haveLink(link1));
	EXPECT_EQ(true,outputTerminal_1->haveLink(link1));

	//LINK NODE 2 & NODE3
	FD::UILink *link2 = m_UINetwork->newLink(outputTerminal_2,inputTerminal_3);
	EXPECT_NE(link2,(FD::UILink*)NULL);
	EXPECT_EQ(link2->getFromTerminal(),outputTerminal_2);
	EXPECT_EQ(link2->getToTerminal(),inputTerminal_3);

	//MAKE SURE NODES ARE AWARE OF THE NEW LINK
	EXPECT_EQ(true,inputTerminal_3->haveLink(link2));
	EXPECT_EQ(true,outputTerminal_2->haveLink(link2));

	//DELETE NODE2 (MIDDLE)
	delete node2;

	//LOOK FOR TERMINAL REMOVED
	EXPECT_EQ(UINodeEventReceiver2.m_notifyTerminalRemovedVector.size(),2);

	//MAKE SURE LINKS WERE REMOVED FROM TERMINALS
	EXPECT_EQ(false,inputTerminal_3->haveLink(link2));
	EXPECT_EQ(false,outputTerminal_2->haveLink(link2));
	EXPECT_EQ(false,inputTerminal_2->haveLink(link1));
	EXPECT_EQ(false,outputTerminal_1->haveLink(link1));

	//VERIFY IF LINKS WERE REMOVED FROM THE NETWORK
	EXPECT_EQ(false,m_UINetwork->haveLink(link1));
	EXPECT_EQ(false,m_UINetwork->haveLink(link2));
	
	delete node1;
	delete node3;

}
