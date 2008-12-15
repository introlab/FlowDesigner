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
	
	//Add ourself to the node's terminal
	if (node)
	{
		node->addTerminal(this);
	}
	
}

UITerminal::~UITerminal()
{



   //although this is weird, it has to be like that since the destroyed link removes
   //itself from the connection list
   while (connections.size())
   {
	  if (node && node->getNetwork())
	  {
		//cerr<<"~UITerminal() Remove link"<<endl;
		node->getNetwork()->removeLink(connections[0]);	  
	  }
	  else
	  {
		  //This is a strange case where we are not part of a network
		  //or a Node : Testing only terminal functionalities
		  break;
	  }
   }

   //Make sure we are removed from the node's terminal list
   if (node)
   {
	//cerr<<"~UITerminal() Remove Terminal from node"<<endl;
   	node->removeTerminal(this,false);
   }


   //Remove the net terminal
   //cerr<<"~UITerminal() RemoveNetTerminal"<<endl;
   removeNetTerminal();
}


void UITerminal::connectNetTerminal(UINetTerminal *term)
{
   netTerminal = term;
   if (node && node->getNetwork())
   {
	   node->getNetwork()->setModified();
   }
}

/**connect to a network terminal*/
void UITerminal::disconnectNetTerminal()
{
   netTerminal = NULL;
   if (node && node->getNetwork())
   {
	   node->getNetwork()->setModified();
   }
}

void UITerminal::removeNetTerminal()
{
   	if (netTerminal) {
      delete netTerminal;
      netTerminal = NULL;
   	}
}

bool UITerminal::haveLink(const UILink *link)
{
	for(size_t i = 0; i < connections.size(); i++)
	{
		if (link == connections[i])
		{
			return true;
		}
	}

	return false;
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
