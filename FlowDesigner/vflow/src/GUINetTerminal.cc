#include "GUINetTerminal.h"
#include "GUITerminal.h"
#include "GUINode.h"
#include "GUINetwork.h"
#include <iostream>

using namespace std;

static void create_net_terminal(gchar * str, GUINetTerminal *term)
{

  cout<<"create_net_terminal"<<endl;

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

   string defaultName;
   string prompt;
   gint anchor;
   
   terminal->getItemPos(x,y);
   char *color;
   if (type == INPUT)
   {
      defaultName = "INPUT";
      prompt = "Input name?";
      anchor = GTK_ANCHOR_EAST;
      x -= 10;
      color="red";
   }   
   else if (type == OUTPUT)
   {
      defaultName = "OUTPUT";
      prompt = "Output name";
      anchor = GTK_ANCHOR_WEST;
      x += 10;
      color="blue";
   }   
   else
   {
      defaultName = "CONDITION";
      anchor = GTK_ANCHOR_WEST;
      x += 10;
      color="purple";
   }
   
   if (name == "")
   {
      name = defaultName;
      
      if (type == INPUT || type == OUTPUT)
      {
         GtkWidget *dialog = gnome_request_dialog (FALSE, prompt.c_str(), defaultName.c_str(), 
                                                   20, (GnomeStringCallback)create_net_terminal, this, NULL);
         gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
      }
   }

   //BUG: Is this dangerous? I don't know
   if (name == "")
   {
      cerr << "aborted\n";
      terminal->getNode()->getNetwork()->removeTerminal(this);
      cerr << "disconnecting\n";
      terminal->disconnectNetTerminal();
      //cerr << "throwing\n";
      //throw false;
      cerr<< "returning"<<endl;
      //Now, it doesn't leak, but it's really ugly
      delete this;
      return;
      
   }
   
   item = gnome_canvas_item_new(dynamic_cast<GUINode *>(terminal->getNode())->getGroup(),
                         gnome_canvas_text_get_type(),
                         "x", x,
                         "y", y,
                         "text", name.c_str(),
                         "anchor", anchor ,
                         "fill_color", color,
                         "font", "fixed",
                         NULL);

   gtk_signal_connect(GTK_OBJECT(item), "event",
                      (GtkSignalFunc) net_terminal_handler,
                      this);

}


GUINetTerminal::~GUINetTerminal()
{
   if (item)
      gtk_object_destroy(GTK_OBJECT(item));
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
