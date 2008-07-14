// Copyright (C) 2001 Jean-Marc Valin

#include "UITerminal.h"
#include "UINode.h"
#include "UILink.h"
#include "UINetTerminal.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include <iostream>

using namespace std;

namespace FD {

//@implements UIClasses

UITerminal::UITerminal (ItemInfo *terminalInfo, UINode *_node, bool _isInput, 
						double _x, double _y)
   : node(_node)
   , x(_x)
   , y(_y)
   , isInput(_isInput)
   , netTerminal(NULL)
{
	name = terminalInfo->name;
	type = terminalInfo->type;
	description = terminalInfo->description;
}

UITerminal::~UITerminal() 
{
   //although this is wierd, it has to be like that since the destroyed link removes 
   //itself from the connection list
   while (connections.size()) {
   	  node->getNetwork()->removeLink(connections[0]);
   }

   //Remove the net terminal
   removeNetTerminal();
}


void UITerminal::connectNetTerminal(UINetTerminal *term) 
{
   netTerminal = term;
   node->getNetwork()->setModified();
}

/**connect to a network terminal*/
void UITerminal::disconnectNetTerminal() 
{
   netTerminal = NULL;
   node->getNetwork()->setModified();
}

void UITerminal::removeNetTerminal()
{
   	if (netTerminal) {
      delete netTerminal;
      netTerminal = NULL;
   	}
}

}//namespace FD
