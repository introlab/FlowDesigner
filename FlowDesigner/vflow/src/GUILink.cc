// Copyright (C) 2001 Jean-Marc Valin

#include "GUILink.h"
#include "GUITerminal.h"
#include "GUINetwork.h"
#include <iostream>

using namespace std;

static gint link_handler (GnomeCanvasItem *item, GdkEvent *event, gpointer data)
{
   return ((GUILink *)(data))->event(event);
}

GUILink::GUILink(UITerminal *_from, UITerminal *_to, char *points_str)
  : UILink (_from, _to, points_str), MIN_POINT_DISTANCE(25.0) {


   GnomeCanvasPoints *points = gnome_canvas_points_new(m_points.size());
   
   int pos = 0;
   
   for (list<GUILinkPoint*>::iterator iter = m_points.begin();
	iter != m_points.end(); iter++) {
      
      points->coords[pos++] = (*iter)->x;
      points->coords[pos++] = (*iter)->y;
      
   }
   
   //gnome_canvas_item_set(item, "points", points, NULL);
   group = GNOME_CANVAS_GROUP (gnome_canvas_item_new (dynamic_cast<GUINetwork *> (net)->getGroup(),
						     gnome_canvas_group_get_type(),
						     "x", 0,
						     "y", 0,
						     NULL));
  
   item = gnome_canvas_item_new(group,
                                gnome_canvas_line_get_type(),
                                "points" , points,
                                "fill_color", "black",
                                "width_units", 2.0,
				"cap_style",GDK_CAP_ROUND,
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

   while (!m_points.empty()) {
     delete m_points.front();
     m_points.pop_front();
   }


}




void GUILink::grab(guint32 etime)
{

  gnome_canvas_item_ungrab(item,etime);
  
  GdkCursor *fleur = gdk_cursor_new(GDK_PLUS);
  
  gnome_canvas_item_grab(item,
			 GDK_POINTER_MOTION_MASK | 
			 GDK_BUTTON_RELEASE_MASK | 
			 GDK_BUTTON_PRESS_MASK,
			 fleur,
			 etime);
  
   gdk_cursor_destroy(fleur);
   
}


gint GUILink::event(GdkEvent *event)
{
   //return TRUE;



  static GUILinkPoint *my_point = NULL;
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

     //cerr<<"button press event"<<endl;
      switch(event->button.button) 
      {
      case 1:
         if (event->button.state & GDK_SHIFT_MASK)
         {
            delete this;
            return TRUE;
         }

	 if (complete) {

	   int pos = 0;

	   GUILinkPoint p1(item_x,item_y);
	   
	   if (m_points.size() == 2) {
	     
	     if (p1.dist(*m_points.front()) > MIN_POINT_DISTANCE &&
		 p1.dist(*m_points.back()) > MIN_POINT_DISTANCE) {
	       
	       //creating new point
	       my_point = new GUILinkPoint(item_x,item_y);

	       list<GUILinkPoint*>::iterator iter_end = m_points.end();

	       //inserting in the list
	       m_points.insert(--iter_end,my_point);
	     }
	     else {
	       //nothing to do
	       my_point = NULL;
	     }
	     
	   }//m_points size == 2
	   else {

	     //adding to the list if far enough
	     my_point = NULL;

	     list<GUILinkPoint*>::iterator iter_end = m_points.end();
	     iter_end--;

	     for (list<GUILinkPoint*>::iterator iter = m_points.begin();
		  iter != iter_end; iter++) {
	      	       
	       list<GUILinkPoint*>::iterator iter1 = iter;
	       
	       GUILinkPoint pfirst ((*iter1)->x,(*iter1)->y);
	       
	       iter1++;
	       
	       GUILinkPoint psecond ((*iter1)->x,(*iter1)->y);
	      
	       double dist1 = p1.dist(pfirst);
	       double dist2 = p1.dist(psecond);
	       

	       if ((dist1 < dist2) && iter != m_points.begin() &&
		   dist1 < MIN_POINT_DISTANCE) {
		 my_point = *iter;
		 break;
	       }

	       if ((dist2 < dist1) && iter1 != iter_end &&
		   dist2 < MIN_POINT_DISTANCE) {
		 my_point = *iter1;
		 break;
	       }

	       if (p1.between(pfirst,psecond)) {
		 
		 if (dist1 > MIN_POINT_DISTANCE &&
		     dist2 > MIN_POINT_DISTANCE) { 

		   //creating new point
		   my_point = new GUILinkPoint(item_x,item_y);
		   
		   //inserting in the list
		   m_points.insert(iter1,my_point);		 
		   break;
		 } 
	       }//between
	       

	     }//for
	   }//else
	 }//complete


         break;
      case 2:
	if (event->button.state & GDK_SHIFT_MASK) {
	  cerr<<"We should add a new input or output!"<<endl;
	}
	break;
      case 3:
	//restoring link to only 2 points
	while (!m_points.empty()) {
	  delete m_points.front();
	  m_points.pop_front();
	}
	
	m_points.push_back(new GUILinkPoint(x1,y1));
	m_points.push_back(new GUILinkPoint(x2,y2));

	points = gnome_canvas_points_new(2);
	
	points->coords[0] = x1;
	points->coords[1] = y1;
	points->coords[2] = x2;
	points->coords[3] = y2;

         gnome_canvas_item_set(item, "points", points, NULL);
	 gnome_canvas_points_unref(points);

	break;

      default:
	break;
      }
      break;
     
   case GDK_MOTION_NOTIFY:
     
     //cerr<<"motion notify event"<<endl;

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
	 
	 //updating our list
	 m_points.front()->x = points->coords[0];
	 m_points.front()->y = points->coords[1];
	 m_points.back()->x = points->coords[2];
	 m_points.back()->y = points->coords[3];


         gnome_canvas_item_set(item, "points", points, NULL);

	 
	 gnome_canvas_points_unref(points);

      }
      else {
	
	if (event->motion.state & GDK_BUTTON1_MASK) {

	  //updating

	  if (my_point) {
	    my_point->x = item_x;
	    my_point->y = item_y;
	  }

	  points = gnome_canvas_points_new(m_points.size());
		  
	  int pos = 0;

	  for (list<GUILinkPoint*>::iterator iter = m_points.begin();
	       iter != m_points.end(); iter++) {

	    points->coords[pos++] = (*iter)->x;
	    points->coords[pos++] = (*iter)->y;

	  }

	  gnome_canvas_item_set(item, "points", points, NULL);

	  gnome_canvas_points_unref(points);
	}
	else {

	  //cursor grab bug...

	  //not complete but not holding button 1
	  gnome_canvas_item_ungrab(item, event->button.time);

	  // commiting suicide
	  delete this;
	  return TRUE;
     
	}

      }

      break;
          
   case GDK_BUTTON_RELEASE:

     //cerr<<"button release event"<<endl;
     
     gnome_canvas_item_ungrab(item, event->button.time);
	
      if (!complete)
      {
	//gnome_canvas_item_ungrab(item, event->button.time);	
	//gnome_canvas_item_ungrab(item, event->button.time);
	
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
      else {

	my_point = NULL;

	net->setModified();

      }

      break;
      
   default:
      break;
   }
        
   return FALSE;
}

void GUILink::move (bool isInput, double dx,double dy)
{

  GUINetwork *my_net = dynamic_cast<GUINetwork*>(net);
  GUINode *my_from = dynamic_cast<GUINode*>(from->getNode());
  GUINode *my_to = dynamic_cast<GUINode*>(to->getNode());
  
  int pos = 0;
  
  if (my_net->isNodeSelected(my_from) &&
      my_net->isNodeSelected(my_to)) {

    if (isInput) {
      GnomeCanvasPoints *points = gnome_canvas_points_new(m_points.size());
      
      x1+=dx;
      y1+=dy;
      x2+=dx;
      y2+=dy;
      
      
      for (list<GUILinkPoint*>::iterator iter = m_points.begin();
	   iter != m_points.end(); iter++) {
	
	(*iter)->x += dx;
	(*iter)->y += dy;
	
	points->coords[pos++] = (*iter)->x;
	points->coords[pos++] = (*iter)->y;
      }
      
      gnome_canvas_item_set(item, "points", points, NULL);
      gnome_canvas_points_unref(points);
    }
  }
  else {

    GnomeCanvasPoints *points = gnome_canvas_points_new(m_points.size());

    if (!isInput) {
      x1+=dx;
      y1+=dy; 
    } 
    else {
      x2+=dx;
      y2+=dy;
    }

    m_points.front()->setxy(x1,y1);
    m_points.back()->setxy(x2,y2);

     for (list<GUILinkPoint*>::iterator iter = m_points.begin();
	iter != m_points.end(); iter++) {
    
        points->coords[pos++] = (*iter)->x;
	points->coords[pos++] = (*iter)->y;
     }

     gnome_canvas_item_set(item, "points", points, NULL);
     gnome_canvas_points_unref(points);
  }
   
  

   
}

void GUILink::update() {

  GnomeCanvasPoints *points = gnome_canvas_points_new(m_points.size());

  m_points.front()->setxy(x1,y1);
  m_points.back()->setxy(x2,y2);

  int pos = 0;
  
  for (list<GUILinkPoint*>::iterator iter = m_points.begin();
       iter != m_points.end(); iter++) {
    
    points->coords[pos++] = (*iter)->x;
    points->coords[pos++] = (*iter)->y;
  }
  
  gnome_canvas_item_set(item, "points", points, NULL);
  gnome_canvas_points_unref(points);
     
}
