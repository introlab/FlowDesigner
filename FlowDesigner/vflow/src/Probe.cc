// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "Probe.h"
#include "net_types.h"
#include "Object.h"
#include <gnome.h>

//DECLARE_NODE(Probe)
NODE_INFO(Probe, "Probe", "INPUT", "OUTPUT", "")

   static void next_click (GtkButton *button, Probe *pr)
{
   pr->next();
}

Probe::Probe(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");

   sem_init(&sem, 0, 0);
}

Probe::~Probe()
{
   //cerr << "Probe destructor\n";

   gdk_threads_enter(); 
   gtk_widget_destroy (window1);
   gdk_threads_leave(); 

   sem_destroy(&sem);
}

void Probe::specificInitialize()
{
   this->Node::specificInitialize();

  gdk_threads_enter(); 


  //GtkWidget *window1;
  //GtkWidget *vbox2;
  GtkWidget *handlebox2;
  GtkWidget *toolbar2;
  GtkWidget *tmp_toolbar_icon;
  //GtkWidget *button16;
  //GtkWidget *button17;
  GtkWidget *button18;
  GtkWidget *button19;
  GtkWidget *scrolledwindow2;
  GtkWidget *canvas2;

  window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_object_set_data (GTK_OBJECT (window1), "window1", window1);
  gtk_window_set_title (GTK_WINDOW (window1), _("window2"));

  vbox2 = gtk_vbox_new (FALSE, 0);
  gtk_widget_ref (vbox2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "vbox2", vbox2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (vbox2);
  gtk_container_add (GTK_CONTAINER (window1), vbox2);

  handlebox2 = gtk_handle_box_new ();
  gtk_widget_ref (handlebox2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "handlebox2", handlebox2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (handlebox2);
  gtk_box_pack_start (GTK_BOX (vbox2), handlebox2, FALSE, FALSE, 0);
  gtk_handle_box_set_snap_edge (GTK_HANDLE_BOX (handlebox2), GTK_POS_LEFT);

  toolbar2 = gtk_toolbar_new (GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);
  gtk_widget_ref (toolbar2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "toolbar2", toolbar2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (toolbar2);
  gtk_container_add (GTK_CONTAINER (handlebox2), toolbar2);

  tmp_toolbar_icon = gnome_stock_pixmap_widget (window1, GNOME_STOCK_PIXMAP_REDO);
  button16 = gtk_toolbar_append_element (GTK_TOOLBAR (toolbar2),
                                GTK_TOOLBAR_CHILD_BUTTON,
                                NULL,
                                _("Next"),
                                NULL, NULL,
                                tmp_toolbar_icon, NULL, NULL);
  gtk_widget_ref (button16);
  gtk_object_set_data_full (GTK_OBJECT (window1), "button16", button16,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button16);

  gtk_signal_connect (GTK_OBJECT (button16), "clicked",
		      GTK_SIGNAL_FUNC (next_click),
		      this);

  gtk_widget_set_sensitive(button16, false);
  //cerr << "registered " << this << endl;

  tmp_toolbar_icon = gnome_stock_pixmap_widget (window1, GNOME_STOCK_PIXMAP_STOP);
  button17 = gtk_toolbar_append_element (GTK_TOOLBAR (toolbar2),
                                GTK_TOOLBAR_CHILD_BUTTON,
                                NULL,
                                _("Break"),
                                NULL, NULL,
                                tmp_toolbar_icon, NULL, NULL);
  gtk_widget_ref (button17);
  gtk_object_set_data_full (GTK_OBJECT (window1), "button17", button17,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button17);

  //gtk_widget_set_sensitive(button17, false);


  tmp_toolbar_icon = gnome_stock_pixmap_widget (window1, GNOME_STOCK_PIXMAP_EXEC);
  button18 = gtk_toolbar_append_element (GTK_TOOLBAR (toolbar2),
                                GTK_TOOLBAR_CHILD_BUTTON,
                                NULL,
                                _("Continue"),
                                NULL, NULL,
                                tmp_toolbar_icon, NULL, NULL);
  gtk_widget_ref (button18);
  gtk_object_set_data_full (GTK_OBJECT (window1), "button18", button18,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button18);

  tmp_toolbar_icon = gnome_stock_pixmap_widget (window1, GNOME_STOCK_PIXMAP_CLOSE);
  button19 = gtk_toolbar_append_element (GTK_TOOLBAR (toolbar2),
                                GTK_TOOLBAR_CHILD_BUTTON,
                                NULL,
                                _("Close"),
                                NULL, NULL,
                                tmp_toolbar_icon, NULL, NULL);
  gtk_widget_ref (button19);
  gtk_object_set_data_full (GTK_OBJECT (window1), "button19", button19,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (button19);




/*  scrolledwindow2 = gtk_scrolled_window_new (NULL, NULL);
  gtk_widget_ref (scrolledwindow2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "scrolledwindow2", scrolledwindow2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (scrolledwindow2);
  gtk_box_pack_start (GTK_BOX (vbox2), scrolledwindow2, TRUE, TRUE, 0);

  gtk_widget_push_visual (gdk_imlib_get_visual ());
  gtk_widget_push_colormap (gdk_imlib_get_colormap ());
  canvas2 = gnome_canvas_new ();
  gtk_widget_pop_colormap ();
  gtk_widget_pop_visual ();
  gtk_widget_ref (canvas2);
  gtk_object_set_data_full (GTK_OBJECT (window1), "canvas2", canvas2,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (canvas2);
  gtk_container_add (GTK_CONTAINER (scrolledwindow2), canvas2);
  gnome_canvas_set_scroll_region (GNOME_CANVAS (canvas2), 0, 0, 100, 100);
  
*/


  gtk_widget_show(window1);

  gdk_threads_leave(); 

  //sleep (1);
  //while(1);
  //return window1;

}

void Probe::reset()
{
   this->Node::reset();
}

void Probe::next()
{
   //cerr << "In Probe::next\n";
   sem_post(&sem);
}

void Probe::trace()
{
   //cerr << "In Probe::trace\n";
   gdk_threads_enter(); 
   gtk_widget_set_sensitive(button16, true);
   gtk_widget_set_sensitive(button17, false);
   gdk_threads_leave(); 

   sem_wait(&sem);

   gdk_threads_enter(); 
   gtk_widget_set_sensitive(button17, true);
   gdk_threads_leave(); 
}

ObjectRef Probe::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {

      NodeInput input = inputs[inputID];
      inputValue = input.node->getOutput(input.outputID,count);
      trace();
      return inputValue;
      
   }
   else 
      throw NodeException (this, "Probe: Unknown output id", __FILE__, __LINE__);
}
