// Copyright (C) 2001 Jean-Marc Valin

#include "GUITerminal.h"
#include "GUINode.h"
#include "GUILink.h"
#include "GUINetTerminal.h"
#include "UIDocument.h"
#include <iostream>

using namespace std;

static gint terminal_handler (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
   return ((GUITerminal *)(data))->event(event);
}

GUITerminal::~GUITerminal()
{
   gtk_object_destroy(GTK_OBJECT(item));
   gtk_object_destroy(GTK_OBJECT(item_text));
}

GUITerminal::GUITerminal (ItemInfo* terminalInfo, UINode *_node, bool _isInput, double _x, double _y)
   : UITerminal(terminalInfo, _node, _isInput, _x, _y)
   , hidden (false)
{

  
  GnomeCanvasGroup *group = dynamic_cast<GUINode *>(node)->getGroup();
   

  //creating the ellipse
  item = gnome_canvas_item_new(group,
			       gnome_canvas_ellipse_get_type(),
			       "x1", x-2.0,
			       "y1", y-2.0,
			       "x2", x+2.0,
			       "y2", y+2.0,
			       "fill_color_rgba", 0x000000FF,
			       "outline_color", "black",
			       "width_units", 2.0,
			       NULL);
  
  gtk_signal_connect(GTK_OBJECT(item), "event",
		     (GtkSignalFunc) terminal_handler,
		     this);

  //creating the tooltip
    




  //creating the text
  if (isInput) {
    item_text = gnome_canvas_item_new(group,
				      gnome_canvas_text_get_type(),
				      "x", x,
				      "y", y,
				      "text", name.c_str(),
				      "anchor", GTK_ANCHOR_WEST ,
				      "fill_color", "blue",
				      "font", "fixed",
				      NULL);
  


  }

  else {
    item_text = gnome_canvas_item_new(group,
				      gnome_canvas_text_get_type(),
				      "x", x,
				      "y", y,
				      "text", name.c_str(),
				      "anchor", GTK_ANCHOR_EAST ,
				      "fill_color", "blue",
				      "font", "fixed",
				      NULL);


  }

  
}


gint GUITerminal::event(GdkEvent *event)
{
  double item_x, item_y;
  //printf("terminal event\n");
  static GUILink *tmp_link = NULL;
  item_x = event->button.x;
  item_y = event->button.y;
  //gnome_canvas_item_w2i(item->parent, &item_x, &item_y);


  switch (event->type) 
    {
    case GDK_BUTTON_PRESS:
      switch(event->button.button) 
        {
        case 1:
	   try {
           if (event->button.state & GDK_CONTROL_MASK || event->button.state & GDK_MOD1_MASK)
	   {
	      if (!isInput && !netTerminal)
		 new GUINetTerminal(this,UINetTerminal::CONDITION,"");
	   }	      
           if (event->button.state & GDK_SHIFT_MASK)
           {
              if (isInput)
              {
                 if (connections.size() == 0 && !netTerminal)
                    new GUINetTerminal(this,UINetTerminal::INPUT,"");
                 //netTerminal = new UINetTerminal(x-10,y,this,UINetTerminal::INPUT);
                 //cerr << "added net input\n";
              } else {
                 if (!netTerminal)
                    new GUINetTerminal(this,UINetTerminal::OUTPUT,"");
                 //netTerminal = new UINetTerminal(x+10,y,this,UINetTerminal::OUTPUT);
                 //cerr << "added net output\n";
              }
              return TRUE;
              
           } else {
              //printf("terminal click\n");

	      static int last_time = 0;

	      //avoiding double click
	      //Dominic Letourneau
	      //oct. 1 2001
	      if (event->button.time > last_time + 1000) {

		//cerr<<"last_time "<<last_time<<endl;
		//cerr<<"current_time "<<event->button.time<<endl;

		last_time = event->button.time;

		double wx;
		double wy;
		wx=x;
		wy=y;
		gnome_canvas_item_i2w(item->parent, &wx, &wy);
		if (isInput)
		  tmp_link = new GUILink(NULL, this);//, wx, wy, wx, wy);
		else
		  tmp_link = new GUILink(this, NULL);//, wx, wy, wx, wy);
		
		tmp_link->grab(event->button.time);
		connections.insert(connections.end(),tmp_link);
		return TRUE;
	      }
           }
	   } catch (bool b)
	   {
	      cerr << "net terminal cancelled\n";
	      return TRUE;
	   }
	   break;
        default:
           break;
        }
      break;
      
    case GDK_MOTION_NOTIFY:
      break;
          
    case GDK_BUTTON_RELEASE:
       break;
       
    default:
       break;
    }
  
  //cout<<"terminate GUITerminal::event"<<endl;

  return FALSE;
}

void GUITerminal::move (double dx,double dy)
{
   int i;
   for (i=0;i<connections.size();i++)
      dynamic_cast<GUILink *>(connections[i])->move(isInput, dx, dy);
}
