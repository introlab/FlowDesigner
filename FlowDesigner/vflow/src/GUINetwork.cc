// Copyright (C) 2001 Jean-Marc Valin

#include "GUINetwork.h"
#include "GUIDocument.h"
#include "GUINode.h"
#include "GUINote.h"
#include "GUITerminal.h"
#include <libxml/tree.h>
#include "GUILink.h"
#include "GUINetPopup.h"
#include "Node.h"
#include "GUINetTerminal.h"
#include "canvas-background.h"
#include "misc.h"
#include "GUINodeTooltip.h"
#include <sstream>
#include "GUINote.h"

/*static gboolean net_canvas_event   (GtkWidget       *widget,
                                     GdkEventButton  *event,
                                     GUINetwork      *net)
{
   return net->buttonEvent(event);
   }*/

static gint background_handler (GnomeCanvasItem *item, GdkEvent *event, GUINetwork      *net)
{

   
  return net->buttonEvent(event);

}

GUINetwork::GUINetwork(UIDocument *_doc, string _name, Type _type)
   : UINetwork(_doc, _name, _type)
   , zoom(1)
   , tooltip(NULL)
   , networkProperties(NULL)
{
   create();
}

GUINetwork::GUINetwork(UIDocument *_doc, xmlNodePtr net)
   : UINetwork(_doc, net, false)
   , zoom(1)
   , tooltip(NULL)
   , networkProperties(NULL)
{
   //cerr << "GUINetwork::GUINetwork\n";
   name = string((char *)xmlGetProp(net, (xmlChar *)"name"));
   char *netType = (char *)xmlGetProp(net, (xmlChar *)"type");
   
   if (!netType)
   {
      type=subnet;
      //cerr << "netType == NILL\n";
   }   
   else {
      //cerr << "netType = " << netType << endl;
      if (netType == string("subnet"))
	 type=subnet;
      else if (netType == string("iterator"))
	 type=iterator;
      else if (netType == string("threaded"))
	 type=threaded;
   }
   //cerr << "type = " << type << endl;

   //cerr << "creating network in GUINetwork::GUINetwork\n";
   create();
   //cerr << "loading...\n";
   load(net);
   //cerr << "loaded\n";
}

GUINetwork::~GUINetwork() 
{
   //It's important that destroyed be set here because is has an effect 
   //on methods that are called indirectly from the destructor.

  //cerr<<"GUINetwork destroyed :"<<destroyed<<" name : "<<getName()<<endl;

  if (!destroyed) {

   
    //Links are deleted through the nodes destructor
    for (int i=0;i<nodes.size();i++) {
      string my_name = nodes[i]->getName();
      //cerr<<"GUINetwork::~GUINetwork deleting node "<<nodes[i]->getName()<<endl;
      delete nodes[i];
      
   }
    
    delete popup;
    gtk_object_destroy(GTK_OBJECT(group));
    gtk_widget_destroy(GTK_WIDGET(scrolledwindow1));

    if (networkProperties) {
      gtk_widget_destroy(GTK_WIDGET(networkProperties));
      networkProperties = NULL;
    }

    destroyed=true;
  }
    
}

void GUINetwork::create()
{
   //cerr << "GUINetwork::create()\n";
  

   //last page will be used.
   //cerr << "GUINetwork::create() name : "<<name<<endl;
   //cerr << "GUINetwork::create() with notebook : "<<document_notebook<<endl;

   //gtk_widget_show (notebook1);

   scrolledwindow1 = gtk_scrolled_window_new (NULL, NULL);

   gtk_object_set_data(GTK_OBJECT(scrolledwindow1), "net", this);

   gtk_widget_ref (scrolledwindow1);

   //gtk_object_set_data_full (GTK_OBJECT (notebook1), "scrolledwindow1", scrolledwindow1,
   //                          (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (scrolledwindow1);
   //gtk_container_add (GTK_CONTAINER (notebook1), scrolledwindow1);

   gdk_rgb_init ();
   gtk_widget_push_visual (gdk_rgb_get_visual ());
   gtk_widget_push_colormap (gdk_rgb_get_cmap ());
   GtkWidget *canvas1 = gnome_canvas_new ();
   gtk_object_set_data_full (GTK_OBJECT (scrolledwindow1), "canvas1", canvas1,
                             (GtkDestroyNotify) gtk_widget_unref);

   gtk_widget_pop_colormap ();
   gtk_widget_pop_visual ();

   

   //gtk_object_set_data_full (GTK_OBJECT (scrolledwindow1), "canvas1", canvas1,
   //                          (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (canvas1);
   gtk_container_add (GTK_CONTAINER (scrolledwindow1), canvas1);
   gnome_canvas_set_scroll_region (GNOME_CANVAS (canvas1), -400, -400, 400, 400);

   
   //add this network to the document
   dynamic_cast<GUIDocument*>(doc)->add_notebook_network(this,scrolledwindow1);


   canvas=GNOME_CANVAS(canvas1);


   GnomeCanvasItem *background = gnome_canvas_item_new(gnome_canvas_root(canvas),
						       gnome_canvas_background_get_type(),
						       NULL);

   gtk_signal_connect(GTK_OBJECT(background), "event",
                      (GtkSignalFunc) background_handler,
                      this);



   //group = gnome_canvas_root(canvas);
   group = GNOME_CANVAS_GROUP (gnome_canvas_item_new (gnome_canvas_root(canvas),
                                                      gnome_canvas_group_get_type(),
                                                      "x", 0.0,
                                                      "y", 0.0,
                                                      NULL));
   popup = new GUINetPopup(doc,this);

   /* gtk_signal_connect (GTK_OBJECT (canvas), "button_press_event",
                       GTK_SIGNAL_FUNC (net_canvas_event),
                       this);
   
   */

   
}


void GUINetwork::updateScroll ()
{
   double x1,y1,x2,y2;
   double mx1=DBL_MAX,my1=DBL_MAX,mx2=-DBL_MAX,my2=-DBL_MAX;
   //gint ix,iy;
   gnome_canvas_get_scroll_region(canvas,&x1,&y1,&x2,&y2);
   if (!nodes.size())
   {
      gnome_canvas_set_scroll_region(canvas, x1,y1,x2,y2);
      gnome_canvas_item_move(GNOME_CANVAS_ITEM(group), 0, 0);
      return;
   }
   for (int i=0;i<nodes.size();i++)
   {
      double xx,yy;
      nodes[i]->getPos(xx,yy);
      mx1=min(mx1,xx);
      my1=min(my1,yy);
      mx2=max(mx2,xx);
      my2=max(my2,yy);
   }
   mx1-=400;
   my1-=400;
   mx2+=400;
   my2+=400;

   if ((abs(mx1-x1)>100) || (abs(my1-y1)>100) || (abs(mx2-x2)>100) || (abs(my2-y2)>100))
      gnome_canvas_set_scroll_region(canvas, mx1,my1,mx2,my2);
   else 
      gnome_canvas_set_scroll_region(canvas, x1,y1,x2,y2);
   //gnome_canvas_get_scroll_offsets(canvas,&ix,&iy);
   //gnome_canvas_scroll_to(canvas,ix,iy);

   gnome_canvas_item_move(GNOME_CANVAS_ITEM(group), 0, 0);

}

UINode * GUINetwork::addNode (string type, double xx, double yy)
{
   double x,y;
   double x1,y1,x2,y2;
   gint ix,iy;
   //cerr << "my name is " ;
   //cerr << name << endl;
   //why?
   //gnome_canvas_window_to_world(canvas, xx,yy,&x,&y);

   //x=xx;
   //y=yy;
   //why?
   //gnome_canvas_item_w2i(GNOME_CANVAS_ITEM(group)->parent, &x, &y);

   char newName[256];
   int id=0;
   while (1) {
      id++;
      bool unique = true;
      sprintf (newName,"node_%s_%d",type.c_str(),id);
      for (int i=0;i<nodes.size();i++)
      {
         if (nodes[i]->getName() == newName) 
            unique = false;
      }
      if (unique)
         break;
   }
   UINode *my_newNode = new GUINode (this, newName, type, xx, yy);
   nodes.insert(nodes.end(), my_newNode);
   updateScroll();

   doc->setModified();
   return my_newNode;
}


gboolean GUINetwork::buttonEvent(GdkEvent *event) {
  
  static GnomeCanvasItem *item = NULL;
  static double x,y;
  double item_x, item_y;
  
  item_x = event->button.x;
  item_y = event->button.y;
  
 


   switch (event->type) {

   
   case GDK_BUTTON_PRESS:

     //keeping last click
     x_last_click = item_x;
     y_last_click = item_y;
     
     switch(event->button.button) {
       
     case 1:
    
       if (item) {
	 gtk_object_destroy(GTK_OBJECT(item));
       }

       item = gnome_canvas_item_new (group,
				     gnome_canvas_rect_get_type(),
				     "x1",item_x,
				     "y1",item_y,
				     "x2",item_x,
				     "y2",item_y,
				     "outline_color","blue",NULL);

       selectedNodes.resize(0);

       x = item_x;
       y = item_y;
       
       break;
       
     case 3:

       //update popup
       delete popup;
       popup = new GUINetPopup(doc,this);

       dynamic_cast<GUIDocument*>(doc)->updateSubnet();

       popup->popup(event);

       return TRUE;
       break;
       
     default:
       break;
     }
     break;

   case GDK_MOTION_NOTIFY:
     
     //resizing rectangle
     if (event->motion.state & GDK_BUTTON1_MASK) {
       if (item_x > x) {
	 gnome_canvas_item_set (item, "x2",item_x,NULL);
       }
       else {
	 gnome_canvas_item_set (item, "x1",item_x,"x2",x,NULL);  
       }
       if (item_y > y) {
	 gnome_canvas_item_set (item, "y2",item_y,NULL);
       }
       else {
	 gnome_canvas_item_set (item, "y1",item_y,"y2",y,NULL);
       }

       
       selectedNodes.resize(0);

       double ibx1,iby1,ibx2,iby2;
       gnome_canvas_item_get_bounds (item,
				     &ibx1,
				     &iby1,
				     &ibx2,
				     &iby2);

       for (int i = 0; i < nodes.size(); i++) {

	 GUINode *nodePtr = dynamic_cast<GUINode*>(nodes[i]);
	 if (nodePtr) {

	   double nx1,ny1,nx2,ny2;

	   nodePtr->getBounds(nx1,ny1,nx2,ny2);

	   if (nx1 >= ibx1 && 
	       nx2 <= ibx2 &&
	       ny1 >= iby1 &&
	       ny2 <= iby2) {

	     selectedNodes.push_back(nodePtr);
	    
	     //node inside rectangle
	     nodePtr->select();

	   }
	   else {
	     nodePtr->unselect();
	   }
	 }
       }
     }
     break;

   case GDK_BUTTON_RELEASE:
     //destroying the rectangle

     if (item) {

       for(int i = 0; i < nodes.size(); i++) {

	 GUINode *nodePtr = dynamic_cast<GUINode*>(nodes[i]);
	 
	 if (isNodeSelected(nodePtr)) {
	   nodePtr->select();
	 }
	 else {
	   nodePtr->unselect();
	 }
       }

       gtk_object_destroy(GTK_OBJECT(item));
       item = NULL;
     }

     break;

   case GDK_LEAVE_NOTIFY:      
     break;

   }//switch event
   /*
   switch (event->button) {
    case 1:
       //printf ("button 1\n");
    break;
    case 2:
       //printf ("button 2\n");
       //UIDocument::currentDocument->saveXML("tata.xml");
    break;
    case 3:
       //printf ("button 3\n");
       popup->popup(event);
       //nodeMenu->popup(event);
       //addNode("PS", 100.0,100.0);
       //UIDocument::currentDocument->popupNodeMenu(event);
    break;
    }*/
  return FALSE;
}

void GUINetwork::newNetNotify(const string &cat, const string &type) 
{
   if (type != name) 
      popup->addType(cat,type);
}


UITerminal *GUINetwork::isNearInputTerminal (double &x, double &y)
{
   for (int i=0;i<nodes.size();i++)
   {
      GUINode *theNode = dynamic_cast<GUINode *> (nodes[i]);
      //UINode *node=nodes[i];
      for (int j=0;j<theNode->inputs.size();j++)
         if (dynamic_cast<GUITerminal *> (theNode->inputs[j])->dist(x,y) < 5)
         {
            dynamic_cast<GUITerminal *> (theNode->inputs[j])->getPos(x,y);
            return theNode->inputs[j];
         }
   }
   return NULL;
}

UITerminal *GUINetwork::isNearOutputTerminal (double &x, double &y)
{
   for (int i=0;i<nodes.size();i++)
   {
      GUINode *theNode = dynamic_cast<GUINode *> (nodes[i]);
      //UINode *node=nodes[i];
      for (int j=0;j<theNode->outputs.size();j++)
         if (dynamic_cast<GUITerminal *> (theNode->outputs[j])->dist(x,y) < 5)
         {
            dynamic_cast<GUITerminal *> (theNode->outputs[j])->getPos(x,y);
            return theNode->outputs[j];
         }
   }
   return NULL;
}


UINode *GUINetwork::newNode(UINetwork* _net, xmlNodePtr def) {
  return new GUINode(_net, def);
}


UINode *GUINetwork::newNode(UINetwork* _net, string _name, string _type, double _x, double _y, bool doInit) {
  return new GUINode(_net, _name, _type, _x, _y);
}


/*UITerminal *GUINetwork::newTerminal (string _name, UINode *_node, bool _isInput, double _x, double _y)
{
   return new GUITerminal (_name, _node, _isInput, _x, _y);
}*/

UILink *GUINetwork::newLink (UITerminal *_from, UITerminal *_to, char *str)
{
   return new GUILink (_from, _to, str);
}

UINetTerminal *GUINetwork::newNetTerminal (UITerminal *_terminal, UINetTerminal::NetTermType _type, const string &_name,
					   const string &_objType, const string &_description )
{
  return new GUINetTerminal (_terminal, _type, _name, _objType, _description);
}

void GUINetwork::moveSelectedNodes(double dx, double dy) {

  for (list<GUINode*>::iterator iter = selectedNodes.begin();
       iter != selectedNodes.end(); iter++) {
    (*iter)->selectedMove(dx,dy);
  }


}

bool GUINetwork::isNodeSelected(GUINode *node) {

  for (list<GUINode*>::iterator iter = selectedNodes.begin();
       iter != selectedNodes.end(); iter++) {
    if ((*iter) == node) return true;
  }


  return false;
}

void GUINetwork::clearSelectedNodes() {

  while (!selectedNodes.empty()) {
    GUINode *my_node = selectedNodes.front();
    selectedNodes.pop_front();
    delete my_node;
  }

}

void GUINetwork::emptySelectedNodes() { 

  while (!selectedNodes.empty()) {
    GUINode *my_node = selectedNodes.front();
    my_node->unselect();
    selectedNodes.pop_front();
  }
}

void GUINetwork::addSelectedNode(GUINode *node) {

  bool found = false;

  //prevent adding selectedNode twice
  for (list<GUINode*>::iterator iter = selectedNodes.begin(); 
       iter != selectedNodes.end(); iter++) {
    if ((*iter) == node) found = true;
  }

  if (!found) {
    selectedNodes.push_back(node);
  }

}



void GUINetwork::removeSelectedNode(GUINode *node) {

  for (list<GUINode*>::iterator iter = selectedNodes.begin(); 
       iter != selectedNodes.end(); iter++) {
    if ((*iter) == node) {
      (*iter)->unselect();
      selectedNodes.erase(iter);
      break;
    }
  }
}


void GUINetwork::popTooltip(GUINode *node)
{
   if (tooltip)
   {
      delete tooltip;
      tooltip=NULL;
   }
   if (node)
      tooltip = new GUINodeTooltip(node);
}

void GUINetwork::rename(string newName) {

  try {
    UINetwork::rename(newName);

    setModified();
  }
  catch (BaseException *e) {
    stringstream str;
    e->print(str);
    GtkWidget*  dialog = gnome_warning_dialog (str.str().c_str());
    delete e;
  }
  
}


static void network_properties_apply (GnomePropertyBox *propertybox, gint arg1, GUINetwork *net) {
  if (net) {
    net->applyNetworkProperties();
  }
}



void GUINetwork::showProperties() {

  if (networkProperties) {
    //destroy old network properties dialog
    gtk_widget_destroy(networkProperties);
  }
  
  networkProperties = gnome_property_box_new();  
  gtk_widget_ref (networkProperties);  


  GtkWidget *scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_ref (scrolled_window);
  gtk_object_set_data_full (GTK_OBJECT (networkProperties), "scrolled_window", scrolled_window,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (scrolled_window);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);


  GtkWidget *text_view = gtk_text_view_new ();
  gtk_widget_ref (text_view);

  //text view properties
  gtk_text_view_set_editable (GTK_TEXT_VIEW (text_view), TRUE);
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (text_view), GTK_WRAP_WORD);
  gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (text_view), TRUE);	 

  //set the description		   
  gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_view)),
			    getDescription().c_str(), -1);

  gtk_object_set_data_full (GTK_OBJECT (networkProperties), "text_view", text_view,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (text_view);
  gtk_container_add (GTK_CONTAINER (scrolled_window), text_view);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(text_view),TRUE);


  //description text signal handler
  g_object_connect (G_OBJECT (gtk_text_view_get_buffer(GTK_TEXT_VIEW(text_view))), "signal::changed",
		    GTK_SIGNAL_FUNC(network_description_changed_event), this,NULL);



  GtkWidget *description_label = gtk_label_new (_("Description"));
  gtk_widget_ref (description_label);

  gtk_object_set_data_full (GTK_OBJECT (networkProperties), "description_label", description_label,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (description_label);
   
  //append "Description page"
  gnome_property_box_append_page  (GNOME_PROPERTY_BOX(networkProperties),scrolled_window, description_label);
  
  gtk_signal_connect (GTK_OBJECT (networkProperties), "apply", GTK_SIGNAL_FUNC(network_properties_apply), this);


  //default = nothing has changed
  gnome_property_box_set_modified (GNOME_PROPERTY_BOX(networkProperties),FALSE);

  //show properties dialog
  gtk_widget_show(networkProperties);

}



void network_description_changed_event (GtkTextBuffer *textbuffer, GUINetwork *network) {  
  gnome_property_box_set_modified (GNOME_PROPERTY_BOX(network->networkProperties),TRUE);
}


void GUINetwork::applyNetworkProperties() {

  if (networkProperties) {
    //updating description
    GtkTextView *text_view = GTK_TEXT_VIEW(gtk_object_get_data(GTK_OBJECT(networkProperties), "text_view"));
    GtkTextBuffer* buffer = gtk_text_view_get_buffer(text_view);
    GtkTextIter start, end;
    gtk_text_buffer_get_bounds(buffer, &start, &end);       
    setDescription(gtk_text_buffer_get_text (buffer, &start, &end, TRUE));    
    setModified();
  }
}


UINote* GUINetwork::newNote(const std::string &text, double x, double y, bool visible) {
  return new GUINote(text,x,y,visible,this);
}
