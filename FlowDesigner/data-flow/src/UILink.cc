// Copyright (C) 2001 Jean-Marc Valin

#include "UILink.h"
#include "UINode.h"
#include "UINetwork.h"
#include "UITerminal.h"
#include <tree.h>

#include "Network.h"
#include <sstream>

//@implements UIClasses

UILink::UILink(UITerminal *_from, UITerminal *_to, char *points_str)
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
   if (points_str)
   {
      stringstream str(points_str);
      while(1)
      {
	 double x, y;
	 str >> x >> y;
	 if (str.eof())
	    break;
	 m_points.push_back(new GUILinkPoint(x,y));
      }
   } else {
      m_points.push_back(new GUILinkPoint(x1,y1));
      m_points.push_back(new GUILinkPoint(x2,y2));
   }
}

UILink::~UILink()
{
   list<GUILinkPoint*>::iterator it = m_points.begin();
   while (it != m_points.end())
   {
      delete *it;
      it++;
   }
   if (to) 
      to->disconnect(this);
   if (from) 
      from->disconnect(this);
   if (complete)
      net->removeLink(this);
}



void UILink::saveXML(xmlNode *root)
{
   xmlNodePtr tree;
   if (m_points.size()<=2)
      tree = xmlNewChild(root, NULL, (CHAR *)"Link", NULL);
   else {
      stringstream str;
      list<GUILinkPoint*>::iterator it = m_points.begin();
      while(it != m_points.end())
      {
	 str << (*it)->x << " " << (*it)->y << " ";
	 it++;
      }
      tree = xmlNewChild(root, NULL, (CHAR *)"Link", (xmlChar*)str.str().c_str());
   }
       
   xmlSetProp(tree, (CHAR *)"from", (CHAR *)from->getNode()->getName().c_str());
   xmlSetProp(tree, (CHAR *)"output", (CHAR *)from->getName().c_str());
   xmlSetProp(tree, (CHAR *)"to", (CHAR *)to->getNode()->getName().c_str());
   xmlSetProp(tree, (CHAR *)"input", (CHAR *)to->getName().c_str());
}

void UILink::build(Network *net)
{
   if (!to || !from)
      throw new GeneralException("Link is not connected at both endes", __FILE__, __LINE__);
   if (!to->getNode() || !from->getNode())
      throw new GeneralException("Cannot find node associated with link", __FILE__, __LINE__);
   net->connect(to->getNode()->getName(), to->getName(), 
		from->getNode()->getName(), from->getName());
}

void UILink::genCode(ostream &out)
{
   if (!to || !from)
      throw new GeneralException("Link is not connected at both endes", __FILE__, __LINE__);
   if (!to->getNode() || !from->getNode())
      throw new GeneralException("Cannot find node associated with link", __FILE__, __LINE__);
   //No need for run-time check, I think
   out << "   net->connect(\"" << to->getNode()->getName() << "\", \"" << to->getName()
       << "\", \"" << from->getNode()->getName() << "\", \"" << from->getName() << "\");\n\n";
}
