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

#include "PlotProbe.h"
#include "net_types.h"
#include "Object.h"
#include <gnome.h>
#include <strstream>

//DECLARE_NODE(Probe)
NODE_INFO(PlotProbe, "Probe", "INPUT", "OUTPUT", "")


PlotProbe::PlotProbe(string nodeName, ParameterSet params) 
   : Probe(nodeName, params)
{
}

PlotProbe::~PlotProbe()
{

}

void PlotProbe::specificInitialize()
{
   Probe::specificInitialize();

   gdk_threads_enter(); 
   
   gdk_rgb_init ();
   gtk_widget_push_visual (gdk_rgb_get_visual ());
   gtk_widget_push_colormap (gdk_rgb_get_cmap ());
  canvas1 = gnome_canvas_new_aa ();
   gtk_widget_pop_colormap ();
   gtk_widget_pop_visual ();

   gtk_window_set_default_size(GTK_WINDOW(window1), 400, 300);
   //gtk_window_set_policy (GTK_WINDOW(window1), FALSE, FALSE, TRUE);
   //canvas1 = gtk_scrolled_window_new (NULL, NULL);
   //canvas1 = gtk_layout_new();

   gtk_widget_ref (canvas1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "canvas1", canvas1,
			     (GtkDestroyNotify) gtk_widget_unref);

   gtk_widget_show (canvas1);
   gtk_box_pack_start (GTK_BOX (vbox2), canvas1, TRUE, TRUE, 0);
   gdk_threads_leave(); 
   
   //gtk_box_pack_start (GTK_BOX (vbox2), scrolledwindow2, TRUE, TRUE, 0);
}

void PlotProbe::reset()
{
   Probe::reset();
}


void PlotProbe::display()
{
   /*char probeOut[1000];
   ostrstream out(probeOut, 999);
   
   out << *inputValue;
   cerr << "Probe value = " << *inputValue << endl;
   gdk_threads_enter();
   gnome_less_clear (GNOME_LESS(less1));
   gnome_less_show_string(GNOME_LESS(less1), probeOut);
   gdk_threads_leave(); */
}

