// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINODE_H
#define GUINODE_H

#include "UINode.h"
#include <gnome.h>
#include <string>
#include <vector>
#include <string>
//#include <tree>
#include <fstream>
#include <iostream>
#include <list>
#include "UINetTerminal.h"
#include "UITerminal.h"

class GUINode : public UINode {

protected:
  
   GnomeCanvasGroup *group;
  
   GtkWidget *popupMenu;
  
   bool grab;
  
   bool dragging;
  
   GnomeCanvasItem *nodeRect;
   GnomeCanvasItem *nodeText;

   void initialize_widgets();

public:

  
   GUINode(UINetwork* _net, std::string _name, std::string _type, double x, double y);

   GUINode(UINetwork* _net, xmlNodePtr def);
  
   ~GUINode();
  
   GnomeCanvasGroup * getGroup() {return group;}
  
   void createPopup();
  
   virtual void rename(const std::string &newName);

   gint event(GdkEvent *event);
  
   void doGrab();
  
   void move (double dx,double dy);

   void selectedMove(double dx,double dy);

   void select();

   void unselect();
  
   void getBounds(double &x1, double &y1, double &x2, double &y2);

   void propertiesShow();

   void help();


   virtual void addTerminal(const std::string &_name, UINetTerminal::NetTermType _type, 
			    const std::string &_objType="any", const std::string &_description="No description available");
   

   virtual UINodeParameters *newNodeParameters (UINode *_node, std::string type);

   virtual UILink *newLink (UITerminal *_from, UITerminal *_to);

   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const std::string &_name,
					  const std::string &_objType="any", const std::string &_description="No description available");
   
   virtual void notifyError(const std::string &message);

   virtual void redraw();

   friend class GUINetwork;

};

#endif
