// Copyright (C) 2001 Jean-Marc Valin

#include "UINetTerminal.h"
#include "UITerminal.h"
#include "UINode.h"
#include "UINetwork.h"



UINetTerminal::UINetTerminal(UITerminal *_terminal, NetTermType _type, string _name)
   : name(_name)
   , terminal(_terminal)
   , type(_type)
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
      tree = xmlNewChild(root, NULL, (xmlChar *)"NetInput", NULL);
   else if (type == OUTPUT)
      tree = xmlNewChild(root, NULL, (xmlChar *)"NetOutput", NULL);
   else
      tree = xmlNewChild(root, NULL, (xmlChar *)"NetCondition", NULL);

   xmlSetProp(tree, (xmlChar *)"name", (xmlChar *)name.c_str());
   xmlSetProp(tree, (xmlChar *)"node", (xmlChar *)terminal->getNode()->getName().c_str());
   xmlSetProp(tree, (xmlChar *)"terminal", (xmlChar *)terminal->getName().c_str());

}
