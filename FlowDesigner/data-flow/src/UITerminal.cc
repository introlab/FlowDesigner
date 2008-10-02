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
   //although this is weird, it has to be like that since the destroyed link removes
   //itself from the connection list
   while (connections.size()) {
	  if (node && node->getNetwork())
	  {
		  node->getNetwork()->removeLink(connections[0]);
	  }
	  else
	  {
		  //This is a strange case where we are not part of a network
		  //or a Node : Testing only terminal functionalities
		  break;
	  }
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


void UITerminal::disconnect(UILink *link)
{
   //Now, this should comply to ANSI C++
   std::vector<UILink *>::iterator i=connections.begin();
   while (i != connections.end())
   {
	 if (*i == link)
	 {
	    connections.erase(i);
	    break;
	 }
	 ++i;
   }
}

}//namespace FD
