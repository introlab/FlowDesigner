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
  

  public:


  //test (DL)
  void addTerminal(const string &_name, UINetTerminal::NetTermType _type);
  
  
  GUINode(UINetwork* _net, string _name, string _type, double x, double y);
  
  GUINode(UINetwork* _net, xmlNodePtr def);
  
  ~GUINode();
  
  GnomeCanvasGroup * getGroup() {return group;}
  
  void draw();
  
  void createPopup();
  
  gint event(GdkEvent *event);
  
  void doGrab();
  
  void move (double dx,double dy);
  
  void propertiesShow();
  /*
    void setAsCondition();
    
    void unsetAsCondition();
  */
  //   virtual UITerminal *newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y);
  
  virtual UILink *newLink (UITerminal *_from, UITerminal *_to);
  
  virtual UINetTerminal *newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name);
  
  virtual UINodeParameters *newNodeParameters (UINode *_node, string type);
  
  virtual void notifyError(const string &message);

  friend class GUINetwork;

};

#endif
