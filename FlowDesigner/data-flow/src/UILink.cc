#include "UILink.h"
#include "UINode.h"
#include "UINetwork.h"
#include "UITerminal.h"
#include <tree.h>

#include "Network.h"

UILink::UILink(UITerminal *_from, UITerminal *_to)
   : from(_from)
   , to(_to)
{
   complete = from != NULL && to != NULL;
   if (from)
   {
      net = from->getNode()->getNetwork();
      from->getPos(x1,y1);
      if (!to)
      {
         x2=x1;
         y2=y1;
      }
   }
   if (to)
   {
      net = to->getNode()->getNetwork();
      to->getPos(x2,y2);
      if (!from)
      {
         x1=x2;
         y1=y2;
      }
   }
      //else   cerr << "error...\n";

   if (complete)
   {
      net->addLink(this);
      from->connect(this);
      to->connect(this);
   }
   //net->setModified();
}

UILink::~UILink()
{
   if (to) 
      to->disconnect(this);
   if (from) 
      from->disconnect(this);
   if (complete)
      net->removeLink(this);
}



void UILink::saveXML(xmlNode *root)
{
   xmlNodePtr tree = xmlNewChild(root, NULL, (CHAR *)"Link", NULL);
   /*char tmp[15];
   sprintf (tmp, "%f", float(x1));
   xmlSetProp(tree, (CHAR *)"x1", (CHAR *)tmp);
   sprintf (tmp, "%f", float(y1));
   xmlSetProp(tree, (CHAR *)"y1", (CHAR *)tmp);
   sprintf (tmp, "%f", float(x2));
   xmlSetProp(tree, (CHAR *)"x2", (CHAR *)tmp);
   sprintf (tmp, "%f", float(y2));
   xmlSetProp(tree, (CHAR *)"y2", (CHAR *)tmp);*/

   xmlSetProp(tree, (CHAR *)"from", (CHAR *)from->getNode()->getName().c_str());
   xmlSetProp(tree, (CHAR *)"output", (CHAR *)from->getName().c_str());
   xmlSetProp(tree, (CHAR *)"to", (CHAR *)to->getNode()->getName().c_str());
   xmlSetProp(tree, (CHAR *)"input", (CHAR *)to->getName().c_str());
}

void UILink::build(Network *net)
{
   //cerr << to->getName() << endl;
   //cerr << to->getNode()->getName() << endl;
   //cerr << from->getName() << endl;
   //cerr << from->getNode()->getName() << endl << endl;

   net->connect(to->getNode()->getName(), to->getName(), 
		from->getNode()->getName(), from->getName());

   //cerr << "connected\n";
   //net->connect(const string &currentNodeName,const string &inputName, 
   //                    const string &inputNodeName, const string &outputName)
}
