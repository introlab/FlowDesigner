#include "GUINote.h"
#include "GUINetwork.h"

//Copyright (C) 2004 Dominic Letourneau
using namespace std;
using namespace FD;

static gint gui_note_event_handler (GnomeCanvasItem *item, GdkEvent *event, GUINote *note) {
   return note->event(event);
}


static gboolean  gui_note_key_release_event (GtkWidget *widget, GdkEventKey *event, GUINote *note) {
  note->update_text();
  return FALSE;
}

void GUINote::update_text() {

    GtkTextBuffer* buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(m_textView));

    GtkTextIter start_iter;
    GtkTextIter end_iter;

    gtk_text_buffer_get_bounds(buffer, &start_iter, &end_iter);
        
    gchar*  text_data = gtk_text_buffer_get_text(buffer,&start_iter,&end_iter,TRUE);
    
    string new_text(text_data);

    if(text_data) {
      free(text_data);
    }

  
    m_text = new_text;
    m_network->setModified();
  
}

GUINote::GUINote(const std::string &text, double x, double y, bool visible, UINetwork *net) 
  : UINote(text,x,y,visible), m_network(net), m_group(NULL), m_textView(NULL) {
     
    if (visible) {

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
      /*
      textItem = gnome_canvas_item_new(m_group,
				       gnome_canvas_text_get_type(),
				       "x", 0.0,
				       "y", 0.0,
				       "text", m_text.c_str(),
				       "anchor", GTK_ANCHOR_SOUTH,
				       "fill_color", "black",
				       "font", "sans 12",
				       NULL);
      */

      GtkWidget *window = gtk_scrolled_window_new(NULL,NULL);
      gtk_widget_show(window);


      m_textView = gtk_text_view_new() ;
      gtk_widget_show (m_textView);
      gtk_container_add (GTK_CONTAINER (window), m_textView);
      gtk_text_view_set_editable (GTK_TEXT_VIEW (m_textView), TRUE);
      gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (m_textView), GTK_WRAP_WORD);
      gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (m_textView), TRUE);

      //set the initial text      
      gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (m_textView)),
                               m_text.c_str(), -1);



      textItem = gnome_canvas_item_new(m_group,
				       gnome_canvas_widget_get_type(),
				       "x", 0.0,
				       "y", 0.0,
				       "width",320.0,
				       "height",120.0,
				       "widget",window,
				       "anchor", GTK_ANCHOR_SOUTH,
				       "size-pixels",FALSE,
				       NULL);


      gnome_canvas_item_get_bounds(textItem, &x1,&y1, &x2, &y2);
      
      
      //creating the rect surrounding (yellow)
      rectItem = gnome_canvas_item_new(m_group,
				       gnome_canvas_rect_get_type(),
				       "x1", x1-5,
				       "y1", y1-25,
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

      //register text view signals
      gtk_signal_connect(GTK_OBJECT(m_textView), "key-release-event",
			 (GtkSignalFunc) gui_note_key_release_event,
			 this);


    }
    
}

GUINote::~GUINote() {
  if (m_group) {
    gtk_object_destroy(GTK_OBJECT(m_group));
  }
}

void GUINote::move(double dx, double dy) {
  //moving all the group
  gnome_canvas_item_move(GNOME_CANVAS_ITEM(m_group), dx, dy);

  //update position
  m_x+=dx;
  m_y+=dy;

  m_network->setModified();
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
	 if (event->button.state & GDK_SHIFT_MASK) {

	   //removing note
	   m_network->removeNote(this);
	   m_network->setModified();
	   return TRUE;
	 } else {
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
	   
	   gnome_canvas_item_raise_to_top(item);
	   
	   //get the actual position
	   xtmp = item_x;
	   ytmp = item_y;
	 }
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
