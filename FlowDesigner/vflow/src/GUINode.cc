// Copyright (C) 2001 Jean-Marc Valin

#include "GUINode.h"
#include "GUINetwork.h"
#include "Node.h"
#include "GUIDocument.h"
#include "GUITerminal.h"
#include "GUINodeParameters.h"
#include "GUILink.h"
#include "GUINetTerminal.h"
#include "GUINodeTooltip.h"

#include <math.h>

static gint node_handler (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
   return ((GUINode *)(data))->event(event);
}

int theTime;


GUINode::GUINode(UINetwork* _net, string _name, string _type, double _x, double _y)
   : UINode(_net, _name, _type, _x, _y, false)
   , dragging(false)
   , grab(false)
   , guiParams(NULL)
{
   initialize_widgets();
   createPopup();
}


GUINode::GUINode(UINetwork* _net, xmlNodePtr def)
   : UINode(_net, def,false)
   //: net(_net)
   , dragging(false)
   , grab(false)
   , guiParams(NULL)
{
   initialize_widgets();
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
   //x2 *= .33;
   
   gnome_canvas_item_raise_to_top(item1);
   
   double xx1=x1-15.0;
   double xx2=x2+15.0;
   vector<ItemInfo *> inputname = net->getDocument()->getNetInputs(type);
   vector<ItemInfo *> outputname = net->getDocument()->getNetOutputs(type);
   
   if (inputname.size() > 1)
      for (int i=0;i<inputname.size();i++)
      {
	 double tx1,ty1,tx2,ty2;
	 item1 = gnome_canvas_item_new(group,
				       gnome_canvas_text_get_type(),
				       "x", xx1,
				       "y", 0.0,
				       "text", inputname[i]->name.c_str(),
				       "anchor", GTK_ANCHOR_EAST ,
				       "fill_color", "blue",
				       "font", "fixed",
				       NULL);
	 gnome_canvas_item_move(GNOME_CANVAS_ITEM(item1), 0.0, -15.0*(.5*(inputname.size()-1)-i));
	 gnome_canvas_item_get_bounds(item1, &tx1,&ty1, &tx2, &ty2);
	 //tx2*=.33;
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
				       "text", outputname[i]->name.c_str(),
				       "anchor", GTK_ANCHOR_WEST ,
				       "fill_color", "blue",
				       "font", "fixed",
				       NULL);
	 gnome_canvas_item_move(GNOME_CANVAS_ITEM(item1), 0.0, -15.0*(.5*(outputname.size()-1)-i));
	 gnome_canvas_item_get_bounds(item1, &tx1,&ty1, &tx2, &ty2);
	 //tx2=.33;
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
				 "fill_color_rgba", 0x8cd0af20,
				 "outline_color", "black",
				 "width_units", 2.0,
				 NULL);
   gnome_canvas_item_lower_to_bottom(item2);
   nodeRect=item2;  
   //gnome_canvas_item_set(item2, "fill_color_rgba", 0xff000040, NULL);
   
   for (int i=0;i<inputname.size();i++)
   {
      inputs.insert(inputs.end(), new GUITerminal (inputname[i], this, true, x1-5.0, 
						   -15.0*(.5*(inputname.size()-1)-i)));
   }
   for (int i=0;i<outputname.size();i++)
   {
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
   
   //delete parameters;
   net->removeNode(this);
   gtk_object_destroy(GTK_OBJECT(group));
   destroyed=true;
}

static void node_help (GtkMenuItem *menuitem, gpointer user_data)
{
   ((GUINode *)(user_data))->help();
}

static void node_prop (GtkMenuItem *menuitem, gpointer user_data)
{
   ((GUINode *)(user_data))->propertiesShow();
}

static void node_delete (GtkMenuItem *menuitem, gpointer user_data)
{
   delete ((GUINode *)(user_data));
}


void GUINode::propertiesShow()
{
   if (!guiParams)
      guiParams = new GUINodeParameters (this, type, parameters);
}

void GUINode::help()
{
   string fullPath = UIDocument::findExternal(type + ".cc", "VFLOW_SOURCE", false);
   if (fullPath == "")
      fullPath = UIDocument::findExternal(type + ".cpp", "VFLOW_SOURCE", false);
   if (fullPath == "")
      cerr << "Node help not found\n";
   else
      gnome_help_goto(NULL, const_cast<char *>(fullPath.c_str()));
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


   label = gtk_menu_item_new_with_label("Help");
   gtk_menu_append(GTK_MENU(popupMenu),label);
   gtk_widget_show(label);
   gtk_widget_ref (label);
   gtk_object_set_data_full (GTK_OBJECT (popupMenu), "label",
                             label,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_signal_connect(GTK_OBJECT(label), "activate",
                      (GtkSignalFunc) node_help,
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
	   //delete this;
	   
	   //cloning...
	   //creating new node
	   GUINetwork *my_net = dynamic_cast<GUINetwork*>(net);

	   GUINode *my_node = dynamic_cast<GUINode*>(my_net->addNode(getType(),item_x + 10,item_y + 10));

	   //copying parameters
	   UINodeParameters *params_source = getParameters() ;

	   UINodeParameters *params_destination = new UINodeParameters(my_node,getType());
	   
	   params_destination->copyParameterText(params_source);

	   my_node->setNodeParameters(params_destination);

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

	       //if not already selected
	       if (!dynamic_cast<GUINetwork *>(net)->isNodeSelected(this)) {

		 //selecting node
		 select();
		 
		 //unselect other nodes
		 dynamic_cast<GUINetwork *>(net)->emptySelectedNodes();
		 
		 //adding this one to the selected list
		 dynamic_cast<GUINetwork *>(net)->addSelectedNode(this);
		 
	       }


               gdk_cursor_destroy(fleur);
               dragging = TRUE;
            }
         }
         return TRUE;
         break;
      case 2:
	 dynamic_cast<GUINetwork *> (net)->popTooltip(this);
	 //new GUINodeTooltip(this);
         //return TRUE;
         break;

      case 3:
         gnome_popup_menu_do_popup_modal (popupMenu,NULL,NULL,&(event->button),NULL);
         return TRUE;
         break;
      default:
         break;
      }
      break;

   case GDK_MOTION_NOTIFY:
      /*if (event->button.state & GDK_SHIFT_MASK)
      {
	 dynamic_cast<GUINetwork *>(net)->popTooltip(this);
      } else {
	 dynamic_cast<GUINetwork *>(net)->popTooltip(NULL);
	 }*/
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
	 net->setModified();
      }
      break;
      
   case GDK_ENTER_NOTIFY:
      dynamic_cast<GUINetwork *>(net)->popTooltip(NULL);
      break;
   case GDK_LEAVE_NOTIFY:
      dynamic_cast<GUINetwork *>(net)->popTooltip(NULL);
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
      //dynamic_cast<GUINodeParameters *> (parameters)->show();
      propertiesShow();
      break;
   default:
      break;
   }
  
   return FALSE;
}

void GUINode::move(double dx, double dy)
{
  GUINetwork *my_net = dynamic_cast<GUINetwork *>(net);

  if (my_net->isNodeSelected(this)) {
    my_net->moveSelectedNodes(dx,dy);
  }
  else {
    int i;
    gnome_canvas_item_move(GNOME_CANVAS_ITEM(group), dx, dy);
    for (i=0;i<inputs.size();i++)
      dynamic_cast<GUITerminal *>(inputs[i])->move(dx, dy);
    for (i=0;i<outputs.size();i++)
      dynamic_cast<GUITerminal *>(outputs[i])->move(dx, dy);
  }
}

void GUINode::selectedMove(double dx,double dy) {
  int i;
  gnome_canvas_item_move(GNOME_CANVAS_ITEM(group), dx, dy);
  for (i=0;i<inputs.size();i++)
    dynamic_cast<GUITerminal *>(inputs[i])->move(dx, dy);
  for (i=0;i<outputs.size();i++)
    dynamic_cast<GUITerminal *>(outputs[i])->move(dx, dy);
}



UILink *GUINode::newLink (UITerminal *_from, UITerminal *_to)
{
   return new GUILink (_from, _to);
}

UINetTerminal *GUINode::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, string _name)
{
   return new GUINetTerminal (_terminal, _type, _name);
}

void GUINode::addTerminal(const string &_name, UINetTerminal::NetTermType _type) {

  double x1=0,y1=0,x2=0,y2=0;
  
  gnome_canvas_item_get_bounds(nodeRect, &x1,&y1,&x2,&y2);
  

  ItemInfo info;

  info.name = _name;

 
  switch (_type) {

  case UINetTerminal::INPUT :

    inputs.insert(inputs.end(), new GUITerminal (&info, this, true, x1, y1));
    
    break;

  case UINetTerminal::OUTPUT:

    outputs.insert(outputs.end(), new GUITerminal (&info, this, false, x2,y2 ));
    
    break;

  default:
    break;

  }

  redraw();
  

}

void GUINode::initialize_widgets() {
     
  double x1,y1,x2,y2;
  GnomeCanvasItem *item1,*item2;

  //getting the group
  GnomeCanvasGroup* netGroup = dynamic_cast<GUINetwork *> (net)->getGroup();


  //creating the group
  group = GNOME_CANVAS_GROUP (gnome_canvas_item_new (netGroup,
						     gnome_canvas_group_get_type(),
						     "x", x,
						     "y", y,
						     NULL));

  //creating the node name
  nodeText = gnome_canvas_item_new(group,
				gnome_canvas_text_get_type(),
				"x", 0.0,
				"y", 0.0,
				"text", type.c_str(),
				"anchor", GTK_ANCHOR_CENTER,
				"fill_color", "black",
				"font", "fixed",
				NULL);

  //getting the node bounds
  gnome_canvas_item_get_bounds(nodeText, &x1,&y1, &x2, &y2);      
  gnome_canvas_item_raise_to_top(nodeText);


  vector<ItemInfo *> inputname = net->getDocument()->getNetInputs(type);
  vector<ItemInfo *> outputname = net->getDocument()->getNetOutputs(type);
  

  //creating input items
  for (int i=0;i<inputname.size();i++) {
    //cout<<"insert input"<<endl;
    inputs.insert(inputs.end(), new GUITerminal (inputname[i], this, true, x1,y1));
  }

  //creating output items
  for (int i=0;i<outputname.size();i++) {
    //cout<<"insert output"<<endl;
    outputs.insert(outputs.end(), new GUITerminal (outputname[i], this, false, x2, y2));
  }

 
  //creating rectangle
  nodeRect = gnome_canvas_item_new(group,
				gnome_canvas_rect_get_type(),
				"x1", x1-5,
				"y1", y1-5,
				"x2", x2+5,
				"y2", y2+5,
				"fill_color_rgba", 0x8cd0af20,
				"outline_color", "black",
				"width_units", 2.0,
				NULL);

  gnome_canvas_item_lower_to_bottom(nodeRect);


  gtk_signal_connect(GTK_OBJECT(group), "event",
		     (GtkSignalFunc) node_handler,
		     this);

  //updating boundaries
  redraw();

}


void GUINode::redraw() {
  
  double tx1,ty1,tx2,ty2;
  double rx1,ry1,rx2,ry2;
  double max_inputs = 5;
  double max_outputs = 5;
  double start_y1;
  double start_y2;

  int inSize = inputs.size();
  int outSize = outputs.size();

  //the node Text is the reference
  gnome_canvas_item_get_bounds (nodeText,&tx1,&ty1,&tx2,&ty2);

  //rectangle bounding box
  rx1 = tx1;
  ry1 = ty1;
  rx2 = tx2;
  ry2 = ty2;

  //centering on label
  start_y1 = (ty1 + ty2) / 2.0 - max(0.0,inSize  - 1.0) * 15.0 / 2.0;
  start_y2 = (ty1 + ty2) / 2.0 - max(0.0,outSize - 1.0) * 15.0 / 2.0;


  //finding max size for text + input terminal

  if (inSize > 1) {
    for (int i = 0; i < inSize; i++) {
      max_inputs = max(max_inputs,dynamic_cast<GUITerminal*>(inputs[i])->getWidth() + 10.0);    
    }
  }

  
  //finding max size for text + output terminal
  if (outSize > 1) {
    for (int i = 0; i < outSize; i++) {
      max_outputs = max(max_outputs,dynamic_cast<GUITerminal*>(outputs[i])->getWidth() + 10.0);
    }
  }


  //let's position the inputs
  for (int i = 0; i < inSize; i++) { 

    if (inSize > 1) {
      dynamic_cast<GUITerminal*>(inputs[i])->showName();
    }
    else {
      dynamic_cast<GUITerminal*>(inputs[i])->hideName();
    }
  
    //position text & terminal
    dynamic_cast<GUITerminal*>(inputs[i])->setAbsPos(tx1 - max_inputs, start_y1 + 15 * (i));
  }//inputs


  //let's find the maximum width of the output
  for (int i = 0; i < outSize; i++) {

    if (outSize > 1) {
      dynamic_cast<GUITerminal*>(outputs[i])->showName();
    }
    else {
      dynamic_cast<GUITerminal*>(outputs[i])->hideName();
    }
    //position text & terminal
    dynamic_cast<GUITerminal*>(outputs[i])->setAbsPos(tx2 + max_outputs, start_y2 + 15 * (i));


  }//outputs



  rx1 -= max_inputs; 
  rx2 += max_outputs;

  //dont forget text size
  ry1 = min(start_y1,start_y2) - 10;
  ry2 = min(start_y1,start_y2) + 15.0 * max(0,max(inSize -1,outSize -1)) + 10;

  //updating rectangle
  gnome_canvas_item_set(nodeRect,
			"x1",rx1,
			"y1",ry1,
			"x2",rx2,
			"y2",ry2,
			NULL);


}

void GUINode::notifyError(const string &message)
{
   gnome_canvas_item_set(nodeRect, "fill_color_rgba", 0xC0202020, NULL);
   //dynamic_cast<GUINetwork *> (getNetwork())->updateScroll();
}

void GUINode::select() {

  gnome_canvas_item_set (nodeRect,
			 "fill_color_rgba", 0x8ca0af20,
			 NULL);
}

void GUINode::unselect() {
  
 gnome_canvas_item_set (nodeRect,
			"fill_color_rgba", 0x8cd0af20,
			NULL);
}

void GUINode::getBounds(double &x1, double &y1, double &x2, double &y2) {


  //double gx1,gy1,gx2,gy2;
  //double rx1,ry1,rx2,ry2;

  //getting bounds of the group
  gnome_canvas_item_get_bounds (GNOME_CANVAS_ITEM(group),
				&x1,
				&y1,
				&x2,
				&y2);

/*

  //getting bounds of the group
  gnome_canvas_item_get_bounds (GNOME_CANVAS_ITEM(group),
				&gx1,
				&gy1,
				&gx2,
				&gy2);

  printf("group bounds (%f,%f) : (%f,%f)\n",gx1,gy1,gx2,gy2);


  //getting bounds of the rectangle (relative to the group)
  gnome_canvas_item_get_bounds (GNOME_CANVAS_ITEM(nodeRect),
				&rx1,
				&ry1,
				&rx2,
				&ry2);

  printf("rect bounds (%f,%f) : (%f,%f)\n",rx1,ry1,rx2,ry2);
 


  x1 = gx1 + rx1;
  y1 = gy1 + ry1;
  x2 = gx2 + rx2;
  y2 = gy2 + ry2;
*/

}

void GUINode::rename(const string &newName) {

  UINode::rename(newName);

  //change the type text

  
  gnome_canvas_item_set(nodeText,"text",type.c_str(),NULL);
                             

  redraw();

}
