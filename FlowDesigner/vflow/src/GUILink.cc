#include "GUILink.h"
#include "GUITerminal.h"
#include "GUINetwork.h"


static gint link_handler (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
   return ((GUILink *)(data))->event(event);
}

GUILink::GUILink(UITerminal *_from, UITerminal *_to)
   : UILink (_from, _to)
{
   GnomeCanvasPoints *points = gnome_canvas_points_new(2);
   points->coords[0]=x1;
   points->coords[1]=y1;
   points->coords[2]=x2;
   points->coords[3]=y2;
   
   group = GNOME_CANVAS_GROUP (
      gnome_canvas_item_new (dynamic_cast<GUINetwork *> (net)->getGroup(),
			     gnome_canvas_group_get_type(),
			     "x", 0,
			     "y", 0,
			     NULL));
   item = gnome_canvas_item_new(group,
                                gnome_canvas_line_get_type(),
                                "points" , points,
                                "fill_color", "black",
                                "width_pixels", 2,
                                "last_arrowhead", TRUE,
                                "arrow_shape_a", 9.0,
                                "arrow_shape_b", 15.0,
                                "arrow_shape_c", 5.0,
                                NULL);
   gnome_canvas_points_unref(points);
   
   gtk_signal_connect(GTK_OBJECT(item), "event",
                      (GtkSignalFunc) link_handler,
                      this);

}

GUILink::~GUILink()
{
   gtk_object_destroy(GTK_OBJECT(item));
   gtk_object_destroy(GTK_OBJECT(group));
}




void GUILink::grab(guint32 etime)
{
   GdkCursor *fleur = gdk_cursor_new(GDK_PLUS);
   
   gnome_canvas_item_grab(item,
                          GDK_POINTER_MOTION_MASK | 
                          GDK_BUTTON_RELEASE_MASK,
                          fleur,
                          etime);
   gdk_cursor_destroy(fleur);

}


gint GUILink::event(GdkEvent *event)
{
   //return TRUE;
   double new_x, new_y;
   GdkCursor *fleur;
   double item_x, item_y;
   GnomeCanvasPoints *points;
  
   item_x = event->button.x;
   item_y = event->button.y;
   gnome_canvas_item_w2i(item->parent, &item_x, &item_y);
  
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
     
   case GDK_MOTION_NOTIFY:
      if (!complete && (event->motion.state & GDK_BUTTON1_MASK)) 
      {
         //gtk_object_get(new_line, "points", points, NULL);
         points = gnome_canvas_points_new(2);
         points->coords[0]=x1;
         points->coords[1]=y1;
         points->coords[2]=x2;
         points->coords[3]=y2;
         if (!from)
         {
            x1=item_x;
            y1=item_y;
            points->coords[0]=item_x;
            points->coords[1]=item_y;
         } else {
            x2=item_x;
            y2=item_y;
            points->coords[2]=item_x;
            points->coords[3]=item_y;
         }
         gnome_canvas_item_set(item, "points", points, NULL);
      }
      break;
          
   case GDK_BUTTON_RELEASE:
      if (!complete)
      {
         //gnome_canvas_item_ungrab(item, event->button.time);
         gnome_canvas_item_ungrab(item, event->button.time);
         if (!from)
         {
            UITerminal *term = dynamic_cast<GUINetwork *>(net)->isNearOutputTerminal(x1,y1);
            if (term)
            {
               from = term;
               term->connect(this);
               complete = true;
            } else {
               /*committing suicide*/
               delete this;
               return TRUE;
            }
         } else {
            UITerminal *term = dynamic_cast<GUINetwork *>(net)->isNearInputTerminal(x2,y2);
            if (term)
            {
               to = term;
               if (term->isConnected())
               {
                  delete this;
                  return TRUE;
               }
               term->connect(this);
               complete = true;
            } else {
               /*committing suicide*/
               delete this;
               return TRUE;
            }
         }
         net->addLink(this);
         complete = true;
      }
      break;
      
   default:
      break;
   }
        
   return FALSE;
}

void GUILink::move (bool isInput, double dx,double dy)
{
   GnomeCanvasPoints *points = gnome_canvas_points_new(2);
   if (!isInput)
   {
      x1+=dx;
      y1+=dy;
   } else {
      x2+=dx;
      y2+=dy;
   }
   points->coords[0]=x1;
   points->coords[1]=y1;
   points->coords[2]=x2;
   points->coords[3]=y2;
   gnome_canvas_item_set(item, "points", points, NULL);
}
