#include <limits.h>
#include <gtest/gtest.h>
#include "UITerminal.h"
#include "UIDocument.h"


TEST(UITerminal,ALLOC_TEST)
{
	FD::ItemInfo info;

	info.name = "NONAME";
	info.type = "Int";
	info.value = "";
	info.description = "Integer value";

	//UITerminal (ItemInfo *terminalInfo, UINode *_node, bool _isInput, double _x, double _y);
	FD::UITerminal *term = new FD::UITerminal(&info,NULL,true,11.0,10.0);

	EXPECT_EQ(term->getName(),"NONAME");
	EXPECT_EQ(term->getType(), "Int");
	EXPECT_EQ(term->getDescription(),"Integer value");

	EXPECT_EQ(term->getNode(), (FD::UINode*)(NULL));
	EXPECT_EQ(term->isInputTerminal(), true);
	EXPECT_EQ(term->isConnected(),false);
	EXPECT_EQ(term->getNetTerminal(),(FD::UINetTerminal*)NULL);
	EXPECT_EQ(term->getConnections().size(),0);


	delete term;

}
