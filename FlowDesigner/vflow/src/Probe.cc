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

DECLARE_NODE(Probe)
/*Node
 *
 * @name Probe
 * @category Probe
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name BREAK_AT
 * @parameter_description No description available
 *
 * @parameter_name SHOW
 * @parameter_description No description available
 *
 * @parameter_name SKIP
 * @parameter_description No description available
 *
END*/


static void rename_button(GtkWidget *button, char *str)
{
   gtk_label_set_text(GTK_LABEL(g_list_last(gtk_container_children(GTK_CONTAINER(GTK_BIN(button)->child)))->data), str);
}

static void next_click (GtkButton *button, Probe *pr)
{
   pr->next();
}

static void cont_click (GtkButton *button, Probe *pr)
{
   pr->cont();
}

static void break_click (GtkButton *button, Probe *pr)
{
   pr->setBreak();
}

static void show_hide_click (GtkButton *button, Probe *pr)
{
   pr->show_hide();
}

Probe::Probe(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");

   //sem_init(&sem, 0, 0);
   pthread_mutex_init(&mutex, NULL);
   pthread_mutex_lock(&mutex);
   
   traceEnable=true;
   displayEnable=true;

   skip = 1;
   breakAt=0;
   if (parameters.exist("BREAK_AT"))
   {
      breakAt = dereference_cast<int> (parameters.get("BREAK_AT"));
      if (breakAt == -1)
	 traceEnable=false;
   }
   if (parameters.exist("SHOW"))
   {
      displayEnable = dereference_cast<bool> (parameters.get("SHOW"));
   }

   if (parameters.exist("SKIP"))
   {
      skip = dereference_cast<int> (parameters.get("SKIP"));
   }

}

Probe::~Probe()
{
   //cerr << "Probe destructor\n";

   gdk_threads_enter(); 
   gtk_widget_destroy (window1);
   gdk_threads_leave(); 

   //sem_destroy(&sem);
   pthread_mutex_destroy(&mutex);
}

void Probe::specificInitialize()
{
   this->Node::specificInitialize();


   gdk_threads_enter();

   try {
   //GtkWidget *window1;
   //GtkWidget *vbox2;
   GtkWidget *handlebox2;
   GtkWidget *toolbar2;
   GtkWidget *tmp_toolbar_icon;
   //GtkWidget *button16;
   //GtkWidget *button17;
   
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
   gtk_button_set_relief(GTK_BUTTON(button16), GTK_RELIEF_NONE);
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
   gtk_button_set_relief(GTK_BUTTON(button17), GTK_RELIEF_NONE);
   gtk_widget_show (button17);
   
   if (traceEnable)
      gtk_widget_set_sensitive(button17, false);
   
   gtk_signal_connect (GTK_OBJECT (button17), "clicked",
		       GTK_SIGNAL_FUNC (break_click),
		       this);

   
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
   gtk_button_set_relief(GTK_BUTTON(button18), GTK_RELIEF_NONE);
   gtk_widget_show (button18);
   
   gtk_signal_connect (GTK_OBJECT (button18), "clicked",
		       GTK_SIGNAL_FUNC (cont_click),
		       this);
   
   if (!traceEnable)
      gtk_widget_set_sensitive(button18, false);

   const char *hide_name;
   if (displayEnable)
      hide_name=_("Hide");
   else 
      hide_name=_("Show");

   tmp_toolbar_icon = gnome_stock_pixmap_widget (window1, GNOME_STOCK_PIXMAP_CLOSE);
   button19 = gtk_toolbar_append_element (GTK_TOOLBAR (toolbar2),
					  GTK_TOOLBAR_CHILD_BUTTON,
					  NULL,
					  hide_name,
					  NULL, NULL,
					  tmp_toolbar_icon, NULL, NULL);
   gtk_widget_ref (button19);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button19", button19,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_button_set_relief(GTK_BUTTON(button19), GTK_RELIEF_NONE);
   gtk_widget_show (button19);

   gtk_signal_connect (GTK_OBJECT (button19), "clicked",
		       GTK_SIGNAL_FUNC (show_hide_click),
		       this);


   entry1 = gtk_entry_new_with_max_length (11);
   gtk_entry_set_editable(GTK_ENTRY(entry1),false);
   gtk_widget_ref (entry1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "entry1", entry1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (entry1);
   gtk_toolbar_append_widget (GTK_TOOLBAR (toolbar2), entry1, NULL, NULL);
   

   gtk_widget_show(window1);

   } catch (BaseException *e)
   {
      gdk_threads_leave(); 
      throw e->add(new NodeException(this, "Exception caught in Probe::specifigInitialize", __FILE__, __LINE__));
   }
   gdk_threads_leave(); 

}

void Probe::reset()
{
   this->Node::reset();
}

void Probe::next()
{
   //cerr << "In Probe::next\n";
   //sem_post(&sem);
   pthread_mutex_unlock(&mutex);
}

void Probe::cont()
{
   traceEnable = false;

   //gdk_threads_enter(); 
   gtk_widget_set_sensitive(button18, false);
   gtk_widget_set_sensitive(button17, true);
   //gdk_threads_leave(); 

   next();
}

void Probe::setBreak()
{
   traceEnable = true;

   //gdk_threads_enter(); 
   gtk_widget_set_sensitive(button17, false);
   gtk_widget_set_sensitive(button18, true);
   //gtk_label_set_text(GTK_LABEL(GTK_BIN(button18)->child), "tata");
   //gtk_label_set_text(GTK_LABEL(g_list_last(gtk_container_children(GTK_CONTAINER(GTK_BIN(button18)->child)))->data), "tata");
   //gdk_threads_leave(); 
}



void Probe::show_hide()
{
   displayEnable = !displayEnable;
   if (displayEnable)
   {
      rename_button(button19, "Hide");
   }   
   else
   {
      rename_button(button19, "Show");
   }  
}

void Probe::display()
{
}


void Probe::trace()
{
   gdk_threads_enter(); 
   gtk_widget_set_sensitive(button16, true);
   //gtk_widget_set_sensitive(button17, false);
   gtk_widget_set_sensitive(button18, true);
   gdk_threads_leave(); 

   //sem_wait(&sem);
   pthread_mutex_lock(&mutex);
   
   gdk_threads_enter(); 
   //gtk_widget_set_sensitive(button17, true);
   gtk_widget_set_sensitive(button16, false);
   gtk_widget_set_sensitive(button18, false);
   gdk_threads_leave(); 
}

ObjectRef Probe::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {

      NodeInput input = inputs[inputID];
      inputValue = input.node->getOutput(input.outputID,count);

      if (count % skip == 0)
      {
	 char tmp[16];
	 sprintf (tmp,"%d",count);
	 gdk_threads_enter(); 
	 gtk_entry_set_text(GTK_ENTRY(entry1),tmp);
	 gdk_threads_leave(); 
      }
      
      if (displayEnable && (count % skip == 0))
	 display();
      if (traceEnable && (count % skip == 0) && count >= breakAt)
	 trace();
      return inputValue;
      
   }
   else 
      throw new NodeException (this, "Probe: Unknown output id", __FILE__, __LINE__);
}
