#include "GUINetTerminal.h"
#include "GUITerminal.h"
#include "GUINode.h"

static void create_net_terminal(gchar * str, GUINetTerminal *term)
{
   if (str)
      term->setName(string(str));
}



GUINetTerminal::GUINetTerminal(UITerminal *_terminal, NetTermType _type, string _name)
   : UINetTerminal(_terminal, _type, _name)
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
                                                   20, create_net_terminal, this, NULL);
         gnome_dialog_run_and_close(GNOME_DIALOG(dialog));
      }
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

}


GUINetTerminal::~GUINetTerminal()
{
   gtk_object_destroy(GTK_OBJECT(item));
}