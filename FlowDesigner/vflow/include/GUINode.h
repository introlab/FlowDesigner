// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINODE_H
#define GUINODE_H

#include "UINode.h"
#include <gnome.h>
#include <string>
#include <vector>
#include <string>
#include <tree.h>
#include <fstream>
#include "GUITerminal.h"


class GUINetwork;
class GUINodeParameters;

class GUINode : public UINode {
protected:
  
   GnomeCanvasGroup *group;
  
   GtkWidget *popupMenu;
  
   bool grab;
  
   bool dragging;
  
   GnomeCanvasItem *nodeRect;
   GnomeCanvasItem *nodeText;

   void initialize_widgets();
   void redraw();
   GUINodeParameters *guiParams;

public:

   void addTerminal(const string &_name, UINetTerminal::NetTermType _type);
  
   GUINode(UINetwork* _net, string _name, string _type, double x, double y);
  
   GUINode(UINetwork* _net, xmlNodePtr def);
  
   ~GUINode();
  
   GnomeCanvasGroup * getGroup() {return group;}
  
   void draw();
  
   void createPopup();
  
   void paramClose() {guiParams=NULL;}

   virtual void rename(const string &newName);

   gint event(GdkEvent *event);
  
   void doGrab();
  
   void move (double dx,double dy);

   void selectedMove(double dx,double dy);

   void select();

   void unselect();
  
   void getBounds(double &x1, double &y1, double &x2, double &y2);

   void propertiesShow();

   void help();
  
   virtual UILink *newLink (UITerminal *_from, UITerminal *_to);
  
   virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name);
  
   virtual void notifyError(const string &message);

   friend class GUINetwork;

};

#endif
