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

