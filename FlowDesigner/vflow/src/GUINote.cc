#include "GUINote.h"
#include "GUINetwork.h"

//Copyright (C) 2004 Dominic Letourneau


static gint gui_note_event_handler (GnomeCanvasItem *item, GdkEvent *event, GUINote *note) {
   return note->event(event);
}


GUINote::GUINote(const std::string &text, double x, double y, bool visible, UINetwork *net) 
  : UINote(text,x,y,visible), m_network(net) {
     
    //getting the network group
    GnomeCanvasGroup* netGroup = dynamic_cast<GUINetwork *> (m_network)->getGroup();

    GnomeCanvasItem *textItem = NULL;
    GnomeCanvasItem *rectItem = NULL;

    double x1,x2,y1,y2;
    double xx1,xx2,yy1,yy2;
    double xpos, ypos;
    
    //adding the note group
    m_group = GNOME_CANVAS_GROUP (gnome_canvas_item_new (netGroup,
							 gnome_canvas_group_get_type(),
							 "x", x,
							 "y", y,
							 NULL));


    //creating the text area
    textItem = gnome_canvas_item_new(m_group,
				gnome_canvas_text_get_type(),
				"x", 0.0,
				"y", -32.0,
				"text", m_text.c_str(),
				"anchor", GTK_ANCHOR_SOUTH,
				"fill_color", "black",
				"font", "sans 12",
				NULL);

     gnome_canvas_item_get_bounds(textItem, &x1,&y1, &x2, &y2);


     //creating the rect surrounding (yellow)
     rectItem = gnome_canvas_item_new(m_group,
				      gnome_canvas_rect_get_type(),
				      "x1", x1-5,
				      "y1", y1-5,
				      "x2", x2+5,
				      "y2", y2+5,
				      //"fill_color_rgba", 0xe0e03020,
				      //"fill_color_rgba", 0xe0e06020,
				      "fill_color_rgba", 0xf0f05520,
				      "outline_color", "black",
				      "width_units", 1.0,
				      NULL);

     //raise text in front of the background rectangle
     gnome_canvas_item_raise_to_top(textItem);


     //register event handler
     gtk_signal_connect(GTK_OBJECT(m_group), "event",
			(GtkSignalFunc) gui_note_event_handler,
			this);
  
}

void GUINote::move(double dx, double dy) {
  //moving all the group
  gnome_canvas_item_move(GNOME_CANVAS_ITEM(m_group), dx, dy);

  //update position
  m_x+=dx;
  m_y+=dy;
}


gint GUINote::event(GdkEvent *event) {

  double item_x = event->button.x;
  double item_y = event->button.y;
  static double xtmp = 0;
  static double ytmp = 0;

  GnomeCanvasItem *item=GNOME_CANVAS_ITEM(m_group);


  //converting
  gnome_canvas_item_w2i(item->parent, &item_x, &item_y);
   

  switch (event->type) {
  case GDK_BUTTON_PRESS:

    switch(event->button.button) {
      case 1:
	
	gnome_canvas_item_grab(item,
			       GDK_ENTER_NOTIFY_MASK |
			       GDK_LEAVE_NOTIFY_MASK |
			       GDK_FOCUS_CHANGE_MASK |
			       GDK_POINTER_MOTION_MASK | 
			       GDK_BUTTON_RELEASE_MASK |
			       GDK_BUTTON_PRESS_MASK,
			       //GDK_ALL_EVENTS_MASK,
			       NULL,
			       event->button.time);


	//get the actual position
	xtmp = item_x;
	ytmp = item_y;

	break;
    }//switch event->button.button

    break;
    
  case GDK_MOTION_NOTIFY:
    if (event->motion.state & GDK_BUTTON1_MASK) {

      //look at the movement
      double dx = item_x - xtmp;
      double dy = item_y - ytmp;
       
      //lets move
      move(dx,dy);
      
      //update position for next event
      xtmp = item_x;
      ytmp = item_y;
      
    }
    break;

  case GDK_BUTTON_RELEASE:
    switch(event->button.button) {
    case 1:
      gnome_canvas_item_ungrab(item, event->button.time);
      break;
    }//switch event->button.button

    break;
    
  }//switch event->type
  

  return TRUE;
}
