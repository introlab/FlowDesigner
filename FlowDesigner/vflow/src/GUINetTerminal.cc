// Copyright (C) 2001 Jean-Marc Valin

#include "GUINetTerminal.h"
#include "GUITerminal.h"
#include "GUINode.h"
#include "GUINetwork.h"
#include <iostream>
#include "UIDocument.h"
#include "misc_gui.h"

using namespace std;

static void create_net_terminal(gchar * str, GUINetTerminal *term)
{

   //cout<<"create_net_terminal"<<endl;

   if (str)
      term->setName(string(str));
   else 
      term->setName("");
}

static gint net_terminal_handler (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
   //cerr << "caught button\n";
   return ((GUINetTerminal *)(data))->event(event);
}

GUINetTerminal::GUINetTerminal(UITerminal *_terminal, NetTermType _type, string _name)
   : UINetTerminal(_terminal, _type, _name)
   , item(NULL)
{

  //cerr<<"NetTerminal constructor type : "<<_type<<endl;

   string defaultName;
   string prompt;
   gint anchor;
   
   terminal->getItemPos(x,y);
   char *color;
   if (type == INPUT)
   {
     //defaultName = "INPUT";
     //defaultName = terminal->getName();
     defaultName = find_unique_name(terminal->getName(), _type);
     prompt = "Input name?";
     anchor = GTK_ANCHOR_EAST;
     x -= 10;
     color="red";
   }   
   else if (type == OUTPUT) {
     //defaultName = "OUTPUT";
     //defaultName = terminal->getName();
     defaultName = find_unique_name(terminal->getName(),_type);
     prompt = "Output name";
     anchor = GTK_ANCHOR_WEST;
     x += 10;
     color="blue";
   }   
   else if (type == CONDITION) {
     defaultName = "CONDITION";
     anchor = GTK_ANCHOR_WEST;
     x += 10;
     color="purple";
   }
   
   if (name == "") {

     //set the default object_type and description
     setObjectType(terminal->getType());
     setDescription(terminal->getDescription());


     //prompting user for input
     name = ask_string_dialog(prompt.c_str(), defaultName.c_str());
   }
   
   //find network duplicate terminal names 
   //Dominic Letourneau Feb 1 2002.

   //vector<string> my_terms = terminal->getNode()->getNetwork()->getTerminals(_type);
   //int count = 0;

   vector<UINetTerminal*> my_terminals = terminal->getNode()->getNetwork()->getTerminals();

   for (unsigned int i = 0; i < my_terminals.size(); i++) {
     if (my_terminals[i]->getName() == name 
	 && my_terminals[i] != static_cast<UINetTerminal*>(this)
	 && my_terminals[i]->getType() == type) {
       
       cerr<<"*WARNING* duplicate terminal name : "<<name<<endl;
       
       if (type == INPUT || type == OUTPUT) {
	 //updating input/output
	 cerr<<"*WARNING* input/output removed from node : "<<my_terminals[i]->getTerminal()->getNode()->getName()<<endl;
	 my_terminals[i]->getTerminal()->disconnectNetTerminal();
	 delete my_terminals[i]; //safe ?
       }
       if (type == CONDITION) {
	 cerr<<"*WARNING* input/output removed from node : "<<my_terminals[i]->getTerminal()->getNode()->getName()<<endl;
	 my_terminals[i]->getTerminal()->disconnectNetTerminal();
	 delete my_terminals[i]; //safe ?
       } 
     }
   }




   //FIXME: Is this dangerous? I don't know. But it's ugly at best
   if (name == "")
   {
      //cerr << "aborted\n";
      terminal->getNode()->getNetwork()->removeTerminal(this);
      //cerr << "disconnecting\n";
      terminal->disconnectNetTerminal();
      //cerr << "throwing\n";
      //throw false;
      //cerr<< "returning"<<endl;
      //Now, it doesn't leak, but it's really ugly
      delete this;
      return;
      
   }

   if (type ==OUTPUT || type == INPUT) {
     terminal->getNode()->getNetwork()->interfaceChangeNotify();
     UIDocument *doc = terminal->getNode()->getNetwork()->getDocument();
     doc->updateAllSubnetTerminals(terminal->getNode()->getNetwork()->getName(),getName(),getType(),false);
   }
   item = gnome_canvas_item_new(dynamic_cast<GUINode *>(terminal->getNode())->getGroup(),
                         gnome_canvas_text_get_type(),
                         "x", x,
                         "y", y,
                         "text", name.c_str(),
                         "anchor", anchor ,
                         "fill_color", color,
                         "font", "sans 10",
                         NULL);

   gtk_signal_connect(GTK_OBJECT(item), "event",
                      (GtkSignalFunc) net_terminal_handler,
                      this);

}


GUINetTerminal::~GUINetTerminal()
{
  UIDocument *doc = terminal->getNode()->getNetwork()->getDocument();
  doc->updateAllSubnetTerminals(terminal->getNode()->getNetwork()->getName(),getName(),getType(),true);

  if (item) {
    gtk_object_destroy(GTK_OBJECT(item));
  }
}


gint GUINetTerminal::event(GdkEvent *event)
{
   //return TRUE;
   switch (event->type) 
   {
   case GDK_BUTTON_PRESS:
      switch(event->button.button) 
      {
      case 1:
         if (event->button.state & GDK_SHIFT_MASK)
         {
	   delete this;	  
	   return TRUE;

         }
         break;
         
      default:
         break;
      }
      break;
     
      
   default:
      break;
   }
        
   return FALSE;
}

string GUINetTerminal::find_unique_name(const string &_name, NetTermType _type) {

  //automatic creation of an unique name for net terminals
  //could be better, but it works...
  //Dominic Letourneau Feb 1 2002.
  vector<string> my_terms = terminal->getNode()->getNetwork()->getTerminals(_type);
  int duplicate_count = 0;
 
  for (unsigned int i = 0; i < my_terms.size(); i++) {

    bool found = false;

    if (my_terms[i].size() >= _name.size()) {
      found = true;
      for (unsigned int j = 0; j < _name.size(); j++) {
	if (my_terms[i][j] != _name[j]) {
	  found = false;
	  break;
	}
      } 
    }
    if (found) {
      duplicate_count++;
    } 
  }

  if (duplicate_count > 0) {
    char name_id[10];
    sprintf(&name_id[0],"_%i",duplicate_count);
    return string(_name + string(name_id));
  }
  else {
    return _name;
  }
}

void GUINetTerminal::setAbsPos(double x, double y) {
  gnome_canvas_item_set(item,"x",x,"y",y,NULL);
}
