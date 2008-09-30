#include <limits.h>
#include <gtest/gtest.h>
#include "UIDocument.h"

TEST(UIDocumentTest,NAME_TEST)
{
	FD::UIDocument *doc = new FD::UIDocument("untitled");
	EXPECT_EQ(doc->getName(),"untitled");
}