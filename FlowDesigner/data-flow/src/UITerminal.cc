// Copyright (C) 2001 Jean-Marc Valin

#include "UITerminal.h"
#include "UINode.h"
#include "UILink.h"
#include "UINetTerminal.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include <iostream>

using namespace std;

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
/*   for (int i=0;i<connections.size();i++)
      delete connections[i];
*/
   //although this is wierd, it has to be like that since the destroyed link removes 
   //itself from the connection list
   while (connections.size())
      delete connections[0];

   if (netTerminal)
      delete netTerminal;
}


void UITerminal::export2net (ostream &out)
{
   if (!isInput) 
      return;
   if (connections.size() != 1 && !netTerminal)
   {
      cerr << "input error!\n";
      return;
   }

   if (netTerminal)
   {
      out << "   <input: " << name << ", ALL_NETWORK_INPUTS, " 
          << netTerminal->getName() << ">" << endl;
   } else {
      out << "   <input: " << name << ", " << connections[0]->from->node->getName() 
          << ", " << connections[0]->from->name << ">" << endl;
   }
   //connections[0]
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
