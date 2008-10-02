#include <limits.h>
#include <gtest/gtest.h>
#include "UITerminal.h"
#include "UIDocument.h"
#include "UILink.h"


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

	//POS TEST
	double x,y;
	term->getPos(x,y);
	EXPECT_EQ(x,11.0);
	EXPECT_EQ(y,10.0);


	delete term;

}

TEST(UITerminal,LINK_TEST)
{
	FD::ItemInfo info;

	info.name = "NONAME";
	info.type = "Int";
	info.value = "";
	info.description = "Integer value";

	//CREATE INPUT TERMINAL
	FD::UITerminal *inputTerm = new FD::UITerminal(&info,NULL,true,11.0,10.0);
	//CREATE OUTPUT TERMINAL
	FD::UITerminal *outputTerm = new FD::UITerminal(&info,NULL,false,0,0);

	//CREATE LINK
	FD::UILink *link = new FD::UILink(outputTerm,inputTerm,NULL,1);

	EXPECT_EQ(inputTerm->getConnections().size(),1);
	EXPECT_EQ(outputTerm->getConnections().size(),1);

	//DESTROY LINK
	delete link;

	EXPECT_EQ(inputTerm->getConnections().size(),0);
	EXPECT_EQ(outputTerm->getConnections().size(),0);

	delete inputTerm;
	delete outputTerm;
}
