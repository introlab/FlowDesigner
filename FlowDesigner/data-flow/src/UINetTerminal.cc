#include "UINetTerminal.h"
#include "UITerminal.h"
#include "UINode.h"
#include "UINetwork.h"



UINetTerminal::UINetTerminal(UITerminal *_terminal, NetTermType _type, string _name)
   : terminal(_terminal)
   , type(_type)
   , name(_name)
{


   terminal->getNode()->getNetwork()->addTerminal(this);
   terminal->connectNetTerminal (this);
}

UINetTerminal::~UINetTerminal()
{
   terminal->getNode()->getNetwork()->removeTerminal(this);
   terminal->disconnectNetTerminal();
}

void UINetTerminal::setName(const string &_name)
{
   name = _name;
}

void UINetTerminal::saveXML(xmlNode *root)
{
   xmlNodePtr tree;

   if (type == INPUT)
      tree = xmlNewChild(root, NULL, (CHAR *)"NetInput", NULL);
   else if (type == OUTPUT)
      tree = xmlNewChild(root, NULL, (CHAR *)"NetOutput", NULL);
   else
      tree = xmlNewChild(root, NULL, (CHAR *)"NetCondition", NULL);

   xmlSetProp(tree, (CHAR *)"name", (CHAR *)name.c_str());
   xmlSetProp(tree, (CHAR *)"node", (CHAR *)terminal->getNode()->getName().c_str());
   xmlSetProp(tree, (CHAR *)"terminal", (CHAR *)terminal->getName().c_str());

}
