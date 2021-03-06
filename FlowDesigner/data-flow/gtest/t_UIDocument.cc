
#include "t_GenericUIFixture.h"

typedef GenericUIFixture<FD::UIDocument> TestUIDocument;

TEST_F(TestUIDocument,ALLOC_TEST)
{
	UIDocumentEventReceiver eventReceiver(m_UIDocument);
	
	delete m_UIDocument;
	
	EXPECT_EQ(eventReceiver.m_notifyDestroyedVector.size(),1);
	
	//TESTING IF THE DOCUMENT POINTERS WILL AUTOMATICALLY SET TO NULL
	//BY THE TEST FIXTURE
	EXPECT_EQ(m_UIDocument,(FD::UIDocument*)NULL);
	EXPECT_EQ(m_UINetwork,(FD::UINetwork*)NULL);
}

TEST_F(TestUIDocument,DOC_PATH_TEST)
{
	UIDocumentEventReceiver eventReceiver(m_UIDocument);
	
	m_UIDocument->setComments("No comment.");
	EXPECT_EQ(eventReceiver.m_notifyCommentsChangedVector.size(),1);
	
	m_UIDocument->setFullPath("/path/document.n");
	EXPECT_EQ(eventReceiver.m_notifyPathChangedVector.size(),1);
	EXPECT_EQ(eventReceiver.m_notifyNameChangedVector.size(),1);
	EXPECT_EQ(m_UIDocument->getName(),"document.n");
}

TEST_F(TestUIDocument,ADD_REMOVE_NETWORK_TEST)
{
	UIDocumentEventReceiver eventReceiver(m_UIDocument);
	
	//ADD ALREADY EXISTING NETWORK (WILL THROW EXCEPTION)
	EXPECT_THROW(m_UIDocument->addNetwork("MAIN",FD::UINetwork::subnet), FD::BaseException*);
	
	//ADD VALID NETWORK
	FD::UINetwork* net = m_UIDocument->addNetwork("SUBNET",FD::UINetwork::subnet);
	EXPECT_NE(net,(FD::UINetwork*)NULL);
	EXPECT_EQ(net,m_UIDocument->getNetworkNamed("SUBNET"));
	EXPECT_EQ(eventReceiver.m_notifyNetworkAddedVector.size(),1);
	
	//DELETE NETWORK SHOULD BE REMOVED FROM DOCUMENT
	delete net;
	EXPECT_NE(net,m_UIDocument->getNetworkNamed("SUBNET"));
	EXPECT_EQ(eventReceiver.m_notifyNetworkRemovedVector.size(),1);
}

TEST_F(TestUIDocument,DOCUMENT_UIREPOSITORY_UPDATE_TEST)
{
	UIDocumentEventReceiver eventReceiver(m_UIDocument);
	
	//CREATE SUBNET
	FD::UINetwork* net = m_UIDocument->addNetwork("SUBNET",FD::UINetwork::subnet);
	EXPECT_NE(net,(FD::UINetwork*)NULL);
	EXPECT_EQ(net,m_UIDocument->getNetworkNamed("SUBNET"));
	EXPECT_EQ(eventReceiver.m_notifyNetworkAddedVector.size(),1);
	
	//VERIFY IF THE SUBNET EXISTS IN THE REPOSITORY AFTER CREATION
	FD::UINodeRepository &repository = m_UIDocument->getRepository();
	FD::NodeInfo *info = repository.findNode("SUBNET");
	EXPECT_NE(info,(FD::NodeInfo*) NULL);
	
	//CREATE A NODE IN THE SUBNET
	FD::UINode* node = net->createNode("NODE1","TYPE",0,0);
	EXPECT_NE((FD::UINode*)NULL,node);
	
	//CREATE TERMINAL (OUTPUT)
	FD::UITerminal *term1 = node->addTerminal("TERM1",FD::UINetTerminal::OUTPUT,"any","No description available");
	EXPECT_NE((FD::UITerminal*)NULL,term1);
	
	//CREATE TERMINAL (INPUT)
	FD::UITerminal *term2 = node->addTerminal("TERM2",FD::UINetTerminal::INPUT,"any","No description available");
	EXPECT_NE((FD::UITerminal*)NULL,term2);
	
	//CREATE NET TERMINAL (OUTPUT)
	FD::UINetTerminal *netTerm1 = net->createNetTerminal(term1,FD::UINetTerminal::OUTPUT, "OUTPUT_NAME", "OUTPUT_TYPE", "OUTPUT_DESCRIPTION");
	EXPECT_NE((FD::UINetTerminal*)NULL,netTerm1);
	
	//CREATE NET TERMINAL (INPUT)
	FD::UINetTerminal *netTerm2 = net->createNetTerminal(term2,FD::UINetTerminal::INPUT, "INPUT_NAME", "INPUT_TYPE", "INPUT_DESCRIPTION");
	EXPECT_NE((FD::UINetTerminal*)NULL,netTerm2);

	//VERIFY IF LOCAL REPOSITORY IS UPDATED
	info = repository.findNode("SUBNET");
	ASSERT_NE(info,(FD::NodeInfo*) NULL);
	std::vector<FD::ItemInfo *> inputs = info->inputs;
	std::vector<FD::ItemInfo *> outputs = info->outputs;
	ASSERT_EQ(1,inputs.size());
	ASSERT_EQ(1,outputs.size());	
	EXPECT_EQ(inputs[0]->name, "INPUT_NAME");
	EXPECT_EQ(outputs[0]->name, "OUTPUT_NAME");
	EXPECT_EQ(inputs[0]->type, "INPUT_TYPE");
	EXPECT_EQ(outputs[0]->type,"OUTPUT_TYPE");
	EXPECT_EQ(inputs[0]->value, "");
	EXPECT_EQ(outputs[0]->value,"");
	EXPECT_EQ(inputs[0]->description, "INPUT_DESCRIPTION");
	EXPECT_EQ(outputs[0]->description,"OUTPUT_DESCRIPTION");
	
	//REMOVE INPUT TERMINAL AND VERIFY IF LOCAL REPOSITORY IS UPDATED
	delete netTerm2;
	info = repository.findNode("SUBNET");
	ASSERT_NE(info,(FD::NodeInfo*) NULL);
	inputs = info->inputs;
	outputs = info->outputs;
	ASSERT_EQ(0,inputs.size());
	ASSERT_EQ(1,outputs.size());	
	
	//REMOVE OUTPUT TERMINAL AND VERIFY IF LOCAL REPOSITORY IS UPDATED
	delete netTerm1;
	info = repository.findNode("SUBNET");
	ASSERT_NE(info,(FD::NodeInfo*) NULL);
	inputs = info->inputs;
	outputs = info->outputs;
	ASSERT_EQ(0,inputs.size());
	ASSERT_EQ(0,outputs.size());	
	
}	

TEST_F(TestUIDocument,DOCUMENT_INCLUDED_SUBNET_RENAMED)
{
	UIDocumentEventReceiver eventReceiver(m_UIDocument);
	
	//CREATE SUBNET
	FD::UINetwork* net = m_UIDocument->addNetwork("SUBNET",FD::UINetwork::subnet);
	EXPECT_NE(net,(FD::UINetwork*)NULL);
	EXPECT_EQ(net,m_UIDocument->getNetworkNamed("SUBNET"));
	EXPECT_EQ(eventReceiver.m_notifyNetworkAddedVector.size(),1);
	
	//ADD SUBNET AS A NODE IN MAIN NETWORK
	FD::UINode *node = m_UINetwork->createNode("NAME","SUBNET",0,0);
	UINodeEventReceiver nodeEventReceiver(node);
	
	ASSERT_NE(node,(FD::UINode*)NULL);
	FD::UINodeRepository &repository = m_UIDocument->getRepository();	
	EXPECT_NE(repository.findNode("SUBNET"),(FD::NodeInfo*)NULL);
	
	//CHANGE SUBNET NAME
	net->rename("SUBNET_NEWNAME");
	EXPECT_EQ(repository.findNode("SUBNET"),(FD::NodeInfo*)NULL);
	EXPECT_NE(repository.findNode("SUBNET_NEWNAME"),(FD::NodeInfo*)NULL);
	
	EXPECT_EQ(node->getType(),"SUBNET_NEWNAME");
	EXPECT_EQ(nodeEventReceiver.m_notifyTypeChangedVector.size(),1);
}	

TEST_F(TestUIDocument,DOCUMENT_XML_LOAD)
{
	FD::UIDocument *testDocument = new FD::UIDocument(std::string(TEST_DOCUMENT_DIR) + "t_Network1.n");
	UIDocumentEventReceiver eventReceiver(testDocument);
	testDocument->load();
	
	FD::UINetwork *MAIN = testDocument->getNetworkNamed("MAIN");
	ASSERT_NE(MAIN, (FD::UINetwork*) (NULL) );
	
	FD::UINetwork *subnet1 = testDocument->getNetworkNamed("subnet1");
	ASSERT_NE(subnet1, (FD::UINetwork*) (NULL) );
			  
	FD::UINetwork *subnet2 = testDocument->getNetworkNamed("subnet2");
	ASSERT_NE(subnet2, (FD::UINetwork*) (NULL) );
	
	//We should have created 3 networks (MAIN, subnet1, subnet2)
	EXPECT_EQ(eventReceiver.m_notifyNetworkAddedVector.size(),3);
	
	delete testDocument;
	
	//We should have deleted 3 networks (MAIN, subnet1, subnet2)
	EXPECT_EQ(eventReceiver.m_notifyNetworkRemovedVector.size(),3);
	
	
}	
