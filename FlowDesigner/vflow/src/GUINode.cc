#include "GUINode.h"
#include "GUINetwork.h"
#include "Node.h"
#include "GUIDocument.h"
#include "GUITerminal.h"
#include "GUINodeParameters.h"
#include "GUILink.h"
#include "GUINetTerminal.h"

static gint node_handler (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
   return ((GUINode *)(data))->event(event);
}

int theTime;


GUINode::GUINode(UINetwork* _net, string _name, string _type, double _x, double _y)
   : UINode(_net, _name, _type, _x, _y, false)
   , dragging(false)
   , grab(false)
{
   parameters = newNodeParameters(this, type);
   
   draw();

   createPopup();
}


GUINode::GUINode(UINetwork* _net, xmlNodePtr def)
   : UINode(_net, def,false)
   //: net(_net)
   , dragging(false)
   , grab(false)
{
   //cerr << "creating params\n";
   parameters = newNodeParameters(this, type);
   //cerr << "loading params\n";
   parameters->load(def);
   //cerr << "drawing node...\n";
   draw();
   //cerr << "creating node popup...\n";

   createPopup();
}

void GUINode::draw()
{
      GnomeCanvasItem *item1, *item2;
   double x1,y1,x2,y2;
   gint ix,iy;
   //cerr << "finding group...\n";
   GnomeCanvasGroup* netGroup = dynamic_cast<GUINetwork *> (net)->getGroup();
   //cerr << "..found\n";
   group = GNOME_CANVAS_GROUP (gnome_canvas_item_new (netGroup,
                                                      gnome_canvas_group_get_type(),
                                                      "x", x,
                                                      "y", y,
                                                      NULL));
   item1 = gnome_canvas_item_new(group,
                                 gnome_canvas_text_get_type(),
                                 "x", 0.0,
                                 "y", 0.0,
                                 "text", type.c_str(),
                                 "anchor", GTK_ANCHOR_CENTER,
                                 "fill_color", "black",
                                 "font", "fixed",
                                 NULL);
   gnome_canvas_item_get_bounds(item1, &x1,&y1, &x2, &y2);
   
      
   gnome_canvas_item_raise_to_top(item1);

   //cerr << "processing inputs/outputs\n";
   {
      double xx1=x1-15.0;
      double xx2=x2+15.0;
      vector<string> inputname;
      vector<string> outputname;
      _NodeFactory *factory = Node::getFactoryNamed(type);
      if (factory)
      {
	 inputname = factory->getInputs();
	 outputname = factory->getOutputs();
      } else {
	 inputname = net->getDocument()->getNetInputs(type); 
	 outputname = net->getDocument()->getNetOutputs(type); 
	 //cerr << "UINode::draw factory not found in simple nodes\n";
      }
      if (inputname.size() > 1)
      for (int i=0;i<inputname.size();i++)
      {
         double tx1,ty1,tx2,ty2;
         item1 = gnome_canvas_item_new(group,
                                          gnome_canvas_text_get_type(),
                                          "x", xx1,
                                          "y", 0.0,
                                          "text", inputname[i].c_str(),
                                          "anchor", GTK_ANCHOR_EAST ,
                                          "fill_color", "blue",
                                          "font", "fixed",
                                          NULL);
         gnome_canvas_item_move(GNOME_CANVAS_ITEM(item1), 0.0, -15.0*(.5*(inputname.size()-1)-i));
         gnome_canvas_item_get_bounds(item1, &tx1,&ty1, &tx2, &ty2);
         x1=min(x1,tx1);
         y1=min(y1,ty1);
         y2=max(y2,ty2);
      }
      if (outputname.size() > 1)
      for (int i=0;i<outputname.size();i++)
      {
         double tx1,ty1,tx2,ty2;
         item1 = gnome_canvas_item_new(group,
                                          gnome_canvas_text_get_type(),
                                          "x", xx2,
                                          "y", 0.0,
                                          "text", outputname[i].c_str(),
                                          "anchor", GTK_ANCHOR_WEST ,
                                          "fill_color", "blue",
                                          "font", "fixed",
                                          NULL);
         gnome_canvas_item_move(GNOME_CANVAS_ITEM(item1), 0.0, -15.0*(.5*(outputname.size()-1)-i));
         gnome_canvas_item_get_bounds(item1, &tx1,&ty1, &tx2, &ty2);
         x2=max(x2,tx2);
         y1=min(y1,ty1);
         y2=max(y2,ty2);
      }   
      item2 = gnome_canvas_item_new(group,
                                 gnome_canvas_rect_get_type(),
                                 "x1", x1-5,
                                 "y1", y1-5,
                                 "x2", x2+5,
                                 "y2", y2+5,
                                 "fill_color_rgba", 0x3cb37120,
                                 "outline_color", "black",
                                 "width_units", 2.0,
                                 NULL);
    nodeRect=item2;  
    //gnome_canvas_item_set(item2, "fill_color_rgba", 0xff000040, NULL);
    //cerr << "creating terminals\n";
      for (int i=0;i<inputname.size();i++)
         inputs.insert(inputs.end(), new GUITerminal (inputname[i], this, true, x1-5.0, 
                                                     -15.0*(.5*(inputname.size()-1)-i)));
      for (int i=0;i<outputname.size();i++)
         outputs.insert(outputs.end(), new GUITerminal (outputname[i], this, false, x2+5.0, 
                                                     -15.0*(.5*(outputname.size()-1)-i)));
   }
   gtk_signal_connect(GTK_OBJECT(group), "event",
                      (GtkSignalFunc) node_handler,
                      this);
}


GUINode::~GUINode()
{
   for (int i=0;i<inputs.size();i++)
      delete inputs[i];
   for (int i=0;i<outputs.size();i++)
      delete outputs[i];
   
   delete parameters;
   net->removeNode(this);
   gtk_object_destroy(GTK_OBJECT(group));
   destroyed=true;
}

static void node_prop (GtkMenuItem *menuitem, gpointer user_data)
{
   ((GUINode *)(user_data))->propertiesShow();
}

static void node_delete (GtkMenuItem *menuitem, gpointer user_data)
{
   delete ((GUINode *)(user_data));
}
/*
static void node_set_cond (GtkMenuItem *menuitem, gpointer user_data)
{
   //gnome_canvas_item_set((GnomeCanvasItem *)(user_data), "fill_color_rgba", 0xff000040, NULL);
   ((GUINode *)(user_data))->setAsCondition();
}
*/
/*void GUINode::setAsCondition()
{
   net->setCondition(this);
   gnome_canvas_item_set(nodeRect, "fill_color_rgba", 0xff000040, NULL);
}

void GUINode::unsetAsCondition()
{
   gnome_canvas_item_set(nodeRect, "fill_color_rgba", 0x3cb37120, NULL);
}
*/
void GUINode::propertiesShow()
{
   dynamic_cast<GUINodeParameters *> (parameters)->show();
}

void GUINode::createPopup()
{
   popupMenu = gtk_menu_new();
   GtkWidget *label = gtk_menu_item_new_with_label("Delete");
   gtk_menu_append(GTK_MENU(popupMenu),label);
   gtk_widget_show(label);
   gtk_widget_ref (label);
   gtk_object_set_data_full (GTK_OBJECT (popupMenu), "label",
                             label,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_signal_connect(GTK_OBJECT(label), "activate",
                      (GtkSignalFunc) node_delete,
                      this);

   /* if (net->isIter())
   {
      label = gtk_menu_item_new_with_label("Set as condition");
      gtk_menu_append(GTK_MENU(popupMenu),label);
      gtk_widget_show(label);
      gtk_widget_ref (label);
      gtk_object_set_data_full (GTK_OBJECT (popupMenu), "label",
                                label,
                                (GtkDestroyNotify) gtk_widget_unref);
      gtk_signal_connect(GTK_OBJECT(label), "activate",
                         (GtkSignalFunc) node_set_cond,
                         this);
   }
   */
   label = gtk_menu_item_new_with_label("Properties");
   gtk_menu_append(GTK_MENU(popupMenu),label);
   gtk_widget_show(label);
   gtk_widget_ref (label);
   gtk_object_set_data_full (GTK_OBJECT (popupMenu), "label",
                             label,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_signal_connect(GTK_OBJECT(label), "activate",
                      (GtkSignalFunc) node_prop,
                      this);
}

void GUINode::doGrab()
{
   GdkCursor *fleur;
   GnomeCanvasItem *item=GNOME_CANVAS_ITEM(group);
   xtmp = x;
   ytmp = y;
   
   fleur = gdk_cursor_new(GDK_FLEUR);
   gnome_canvas_item_grab(item,
                          GDK_POINTER_MOTION_MASK | 
                          GDK_BUTTON_RELEASE_MASK |
                          GDK_BUTTON_PRESS_MASK,
                          //GDK_ALL_EVENTS_MASK,
                          fleur,
                          0);
   gdk_cursor_destroy(fleur);
   dragging = TRUE;
   grab=true;
}


gint GUINode::event(GdkEvent *event)
{
   // static double x, y;
   double new_x, new_y;
   GdkCursor *fleur;
   //static int dragging;
   double item_x, item_y;

   GnomeCanvasItem *item=GNOME_CANVAS_ITEM(group);
   item_x = event->button.x;
   item_y = event->button.y;
   gnome_canvas_item_w2i(item->parent, &item_x, &item_y);
   //cerr << "+";
   switch (event->type) 
   {
   case GDK_BUTTON_PRESS:
      switch(event->button.button) 
      {
      case 1:
         if (event->button.state & GDK_SHIFT_MASK)
         {
            //dynamic_cast<GUINodeParameters *> (parameters)->show();
	    delete this;
	    return true;
         }
         else 
         {
            if (dragging && grab)
            {
               //cerr << "stop drag\n";
               gnome_canvas_item_ungrab(item, event->button.time);
               dragging = FALSE;
               grab=false;
               dynamic_cast<GUINetwork *>(net)->updateScroll();
            } else 
            {
               //cerr << "start drag\n";
               xtmp = item_x;
               ytmp = item_y;
               
               fleur = gdk_cursor_new(GDK_FLEUR);
               gnome_canvas_item_grab(item,
                                      GDK_ENTER_NOTIFY_MASK |
                                      GDK_LEAVE_NOTIFY_MASK |
                                      GDK_FOCUS_CHANGE_MASK |
                                      GDK_POINTER_MOTION_MASK | 
                                      GDK_BUTTON_RELEASE_MASK |
                                      GDK_BUTTON_PRESS_MASK,
                                      //GDK_ALL_EVENTS_MASK,
                                      fleur,
                                      event->button.time);
               gdk_cursor_destroy(fleur);
               dragging = TRUE;
            }
         }
         return TRUE;
         break;

      case 2:
         gnome_popup_menu_do_popup_modal (popupMenu,NULL,NULL,&(event->button),NULL);
         return TRUE;
         break;
      case 3:
         cerr << "button 3 on node\n";
         return TRUE;
         break;
      default:
         break;
      }
      break;

   case GDK_MOTION_NOTIFY:
      if (dragging && (grab || (event->motion.state & GDK_BUTTON1_MASK))) 
      {
         new_x = item_x;
         new_y = item_y;
         move(new_x - xtmp, new_y - ytmp);
         //gnome_canvas_item_move(item, new_x - xtmp, new_y - ytmp);
         x += new_x - xtmp;
         y += new_y - ytmp;

         xtmp = new_x;
         ytmp = new_y;
      }
      break;
          
   case GDK_BUTTON_RELEASE:
      if (!grab)
      {
         //printf ("button release on node\n");
         gnome_canvas_item_ungrab(item, event->button.time);
         dragging = FALSE;
         dynamic_cast<GUINetwork *> (net)->updateScroll();
      }
      break;
   case GDK_2BUTTON_PRESS:
      //cerr << "double-click\n";
      dynamic_cast<GUINodeParameters *> (parameters)->show();
      break;
   default:
      break;
   }
  
   return FALSE;
}

void GUINode::move(double dx, double dy)
{
   int i;
   gnome_canvas_item_move(GNOME_CANVAS_ITEM(group), dx, dy);
   for (i=0;i<inputs.size();i++)
      dynamic_cast<GUITerminal *>(inputs[i])->move(dx, dy);
   for (i=0;i<outputs.size();i++)
      dynamic_cast<GUITerminal *>(outputs[i])->move(dx, dy);
}

UITerminal *GUINode::newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y)
{
   return new GUITerminal (_name, _node, _isInput, _x, _y);
}

UILink *GUINode::newLink (UITerminal *_from, UITerminal *_to)
{
   return new GUILink (_from, _to);
}

UINetTerminal *GUINode::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name)
{
   return new GUINetTerminal (_terminal, _type, _name);
}

UINodeParameters *GUINode::newNodeParameters (UINode *_node, string type)
{
   //cerr << "GUINode::newNodeParameters\n";
   return new GUINodeParameters (_node, type);
}
