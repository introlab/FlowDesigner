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
#include "Vector.h"

//DECLARE_NODE(Probe)
NODE_INFO(PlotProbe, "Probe", "INPUT", "OUTPUT", "BREAK_AT:SHOW:SKIP")


PlotProbe::PlotProbe(string nodeName, ParameterSet params) 
   : Probe(nodeName, params)
   , xmin(0.0)
   , xmax(400)
   , ymin(0.0)
   , ymax(250)
{
}

PlotProbe::~PlotProbe()
{

}

void PlotProbe::specificInitialize()
{
   Probe::specificInitialize();

   gdk_threads_enter(); 

   try {
   GtkWidget *scrolledwindow1 = gtk_scrolled_window_new (NULL, NULL);
   gtk_widget_ref (scrolledwindow1);
   gtk_object_set_data_full (GTK_OBJECT (vbox2), "scrolledwindow1", scrolledwindow1,
                             (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (scrolledwindow1);

   
   gtk_widget_push_visual (gdk_rgb_get_visual ());
   gtk_widget_push_colormap (gdk_rgb_get_cmap ());
   GtkWidget *canvas1 = gnome_canvas_new ();
   gtk_widget_pop_colormap ();
   gtk_widget_pop_visual ();

   gtk_window_set_default_size(GTK_WINDOW(window1), xmax-xmin+20, ymax-ymin+70);
   //gtk_window_set_policy (GTK_WINDOW(window1), FALSE, FALSE, TRUE);
   //canvas1 = gtk_scrolled_window_new (NULL, NULL);
   //canvas1 = gtk_layout_new();

   gtk_widget_ref (canvas1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "canvas1", canvas1,
			     (GtkDestroyNotify) gtk_widget_unref);

   gtk_widget_show (canvas1);

   //gtk_widget_show (canvas1);
   gtk_container_add (GTK_CONTAINER (scrolledwindow1), canvas1);
   //gnome_canvas_set_scroll_region (GNOME_CANVAS (canvas1), -400, -400, 400, 400);


   gtk_box_pack_start (GTK_BOX (vbox2), scrolledwindow1, TRUE, TRUE, 0);
   //gtk_box_pack_start (GTK_BOX (vbox2), canvas1, TRUE, TRUE, 0);
   
   canvas = GNOME_CANVAS(canvas1);


   gnome_canvas_set_scroll_region(canvas, xmin, ymin, xmax, ymax);

   group = GNOME_CANVAS_GROUP (
      gnome_canvas_item_new (gnome_canvas_root(canvas),
			     gnome_canvas_group_get_type(),
			     "x", 0.0,
			     "y", 0.0,
			     NULL));
   

   GnomeCanvasPoints *points;
   points = gnome_canvas_points_new(2);
   points->coords[0]=xmin;
   points->coords[1]=.5*(ymin+ymax);
   points->coords[2]=xmax;
   points->coords[3]=.5*(ymin+ymax);
   
   
   item = gnome_canvas_item_new(group,
                                gnome_canvas_line_get_type(),
                                "points" , points,
                                "fill_color", "black",
                                "width_pixels", 1,
                                NULL);

   gnome_canvas_points_unref(points);

   //gtk_box_pack_start (GTK_BOX (vbox2), scrolledwindow2, TRUE, TRUE, 0);

   } catch (BaseException *e)
   {
      throw e->add(new NodeException(this, "Exception caught in Probe::specifigInitialize", __FILE__, __LINE__));
   }
   
   gdk_threads_leave(); 

}

void PlotProbe::reset()
{
   Probe::reset();
}


void PlotProbe::display()
{
   GnomeCanvasPoints *points;

   pthread_setcanceltype(PTHREAD_CANCEL_DEFERRED, NULL);
   gdk_threads_enter();
   

   Vector<float> &data = object_cast<Vector<float> > (inputValue);
   points = gnome_canvas_points_new(data.size());
   
   double datamin, datamax;
   datamin = datamax = data[0];
   for (int i=0;i<data.size();i++)
   {
      if (data[i] > datamax)
	 datamax = data[i];
      if (data[i] < datamin)
	 datamin = data[i];
   }
   datamax+=.00001;
   for (int i=0;i<data.size();i++)
   {
      //points->coords[2*i]=(100.0*i)/length;
      points->coords[2*i]=xmin+((xmax-xmin)*i)/(data.size()-1);
      points->coords[2*i+1]= ymin + (ymax-ymin)*(1-(data[i]-datamin)/(datamax-datamin));
   }

   gnome_canvas_item_set(item, "points", points, NULL);

   gnome_canvas_points_unref(points);
   
   //cerr << "plot done...\n";

   gdk_threads_leave();
   pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

}

void PlotProbe::show_hide()
{
   Probe::show_hide();
   if (!displayEnable)
   {
      GnomeCanvasPoints *points;
      points = gnome_canvas_points_new(2);
      points->coords[0]=xmin;
      points->coords[1]=.5*(ymin+ymax);
      points->coords[2]=xmax;
      points->coords[3]=.5*(ymin+ymax);
      gnome_canvas_item_set(item, "points", points, NULL);
   
      
      
      gnome_canvas_points_unref(points);
      
   }
}
