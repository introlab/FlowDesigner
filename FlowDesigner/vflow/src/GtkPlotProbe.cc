// Copyright (C) 1999 Jean-Marc Valin

#include "GtkPlotProbe.h"
#include "net_types.h"
#include "Object.h"
#include <gnome.h>
#include "misc.h"

#include "Vector.h"

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "gtkextra/gtkextra.h"

DECLARE_NODE(GtkPlotProbe)
/*Node
 *
 * @name GtkPlotProbe
 * @category Probe
 * @description Plots a vector using GtkPlot
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Same as input
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


static double tick_size(double range)
{
   const double alpha=.75;
   double orig = .2*range;
   double tick=1;
   while (orig>alpha*10)
   {
      tick*=10;
      orig*=.1;
   }
   if (orig>alpha*5)
      tick*=5;
   else if (orig>alpha*2)
      tick*=2;
   while (alpha*orig<.1)
   {
      tick*=.1;
      orig*=10;
   }
   if (alpha*orig<.2)
      tick*=.2;
   else if (alpha*orig<.5)
      tick*=.5;
   return tick;
}

GtkPlotProbe::GtkPlotProbe(string nodeName, ParameterSet params) 
   : Probe(nodeName, params)
   , xmin(0.0)
   , xmax(500)
   , ymin(0.0)
   , ymax(310)  /*Got the magic size!*/
{
}

GtkPlotProbe::~GtkPlotProbe()
{
   
}

void GtkPlotProbe::specificInitialize()
{
   Probe::specificInitialize();
   NO_CANCEL
      gdk_threads_enter(); 

   try {
      gtk_window_set_default_size(GTK_WINDOW(window1), int(xmax-xmin), int(ymax-ymin));
   
      //gtk_window_set_default_size(GTK_WINDOW(window1), int(xmax-xmin+20), int(ymax-ymin+70));



      canvas = gtk_plot_canvas_new(int(xmax), int(ymax), 1.);
      //canvas = gtk_plot_canvas_new(int(xmax+70), int(ymax+70), 1.);

      GTK_PLOT_CANVAS_SET_FLAGS(GTK_PLOT_CANVAS(canvas), GTK_PLOT_CANVAS_DND_FLAGS);
      //GTK_PLOT_CANVAS_SET_FLAGS(GTK_PLOT_CANVAS(canvas), 0);


      gtk_widget_ref (canvas);
      gtk_object_set_data_full (GTK_OBJECT (window1), "canvas", canvas,
				(GtkDestroyNotify) gtk_widget_unref);

      gtk_widget_show (canvas);



      gtk_box_pack_start (GTK_BOX (vbox2), canvas, TRUE, TRUE, 0);

      active_plot = gtk_plot_new_with_size(NULL, .7, .8);
      gtk_widget_ref (active_plot);
      gtk_object_set_data_full (GTK_OBJECT (canvas), "active_plot", active_plot,
				(GtkDestroyNotify) gtk_widget_unref);

      gtk_widget_show(active_plot);

      //activate_plot(active_plot,canvas);
      gtk_plot_set_range(GTK_PLOT(active_plot), 0., 1., 0., 1.);
      //gtk_plot_legends_move(GTK_PLOT(active_plot), .500, .05);
      //gtk_plot_set_legends_border(GTK_PLOT(active_plot), GTK_PLOT_BORDER_NONE, 0);
      gtk_plot_axis_hide_title(GTK_PLOT(active_plot), GTK_PLOT_AXIS_TOP);
      gtk_plot_axis_hide_title(GTK_PLOT(active_plot), GTK_PLOT_AXIS_BOTTOM);
      gtk_plot_axis_hide_title(GTK_PLOT(active_plot), GTK_PLOT_AXIS_RIGHT);
      gtk_plot_axis_hide_title(GTK_PLOT(active_plot), GTK_PLOT_AXIS_LEFT);
      gtk_plot_hide_legends(GTK_PLOT(active_plot));
      gtk_plot_axis_show_ticks(GTK_PLOT(active_plot), GTK_PLOT_AXIS_BOTTOM, 15, 3);
      gtk_plot_axis_set_ticks(GTK_PLOT(active_plot), GTK_PLOT_AXIS_X, 1., 1);
      gtk_plot_axis_set_ticks(GTK_PLOT(active_plot), GTK_PLOT_AXIS_Y, 1., 1);
      gtk_plot_axis_set_visible(GTK_PLOT(active_plot), GTK_PLOT_AXIS_TOP, TRUE);
      gtk_plot_axis_set_visible(GTK_PLOT(active_plot), GTK_PLOT_AXIS_RIGHT, TRUE);
      //gtk_plot_x0_set_visible(GTK_PLOT(active_plot), TRUE);
      //gtk_plot_y0_set_visible(GTK_PLOT(active_plot), TRUE);
      gtk_plot_canvas_add_plot(GTK_PLOT_CANVAS(canvas), GTK_PLOT(active_plot), .15, .06);
      //gtk_plot_axis_set_labels_suffix(GTK_PLOT(active_plot), GTK_PLOT_AXIS_LEFT, "%");
      //gtk_widget_show(active_plot);


      GdkColor color;

      gdk_color_parse("blue", &color);


      dataset = GTK_PLOT_DATA(gtk_plot_data_new());
      gtk_object_ref (GTK_OBJECT(dataset));
      gtk_object_set_data_full (GTK_OBJECT (active_plot), "dataset", dataset,
				(GtkDestroyNotify) gtk_widget_unref);

      gtk_plot_add_data(GTK_PLOT(active_plot), dataset);
      gtk_widget_show(GTK_WIDGET(dataset));
      gtk_plot_data_set_points(dataset, &x[0], &y[0], &dx[0], &dy[0], 0);
      gtk_plot_data_set_line_attributes(dataset,
					GTK_PLOT_LINE_SOLID,
					1, &color);



   } catch (BaseException *e)
   {
      gdk_threads_leave();
      SET_CANCEL
	 throw e->add(new NodeException(this, "Exception caught in Probe::specifigInitialize", __FILE__, __LINE__));
   }
   
   gdk_threads_leave();
   SET_CANCEL

      }

void GtkPlotProbe::reset()
{
   Probe::reset();
}


void GtkPlotProbe::display()
{
   GnomeCanvasPoints *points;
   NO_CANCEL;
   gdk_threads_enter();
   
   Vector<float> &data = object_cast<Vector<float> > (inputValue);
   int inputLength = data.size();
   if (inputLength>x.size())
   {
      x.resize(inputLength);
      y.resize(inputLength);
      dx.resize(inputLength);
      dy.resize(inputLength);
   }
   gdouble dmin=data[0], dmax=data[0];
   for (int i=0;i<inputLength;i++)
   {
      x[i] = i;
      y[i] = data[i];
      if (y[i] > dmax)
	 dmax = y[i];
      if (y[i] < dmin)
	 dmin=y[i];
      dx[i]=1;
      dy[i]=1;
   }
   //gtk_plot_axis_set_ticks(GTK_PLOT(active_plot), GTK_PLOT_AXIS_X, float(inputLength)/5.0, 1);
   //gtk_plot_axis_set_ticks(GTK_PLOT(active_plot), GTK_PLOT_AXIS_Y, (dmax-dmin)/5, 1);

   gtk_plot_axis_set_ticks(GTK_PLOT(active_plot), GTK_PLOT_AXIS_X, tick_size(inputLength), 1);
   gtk_plot_axis_set_ticks(GTK_PLOT(active_plot), GTK_PLOT_AXIS_Y, tick_size(dmax-dmin), 1);

   gtk_plot_set_range(GTK_PLOT(active_plot), 0, inputLength-1, dmin, dmax);
   gtk_plot_data_set_points(dataset, &x[0], &y[0], &dx[0], &dy[0], inputLength);

   gtk_widget_queue_draw (canvas);

   gdk_threads_leave();
   SET_CANCEL;
   
}

void GtkPlotProbe::show_hide()
{
   Probe::show_hide();
   if (!displayEnable)
   {
      gtk_plot_data_set_points(dataset, &x[0], &y[0], &dx[0], &dy[0], 0);
      gtk_widget_queue_draw (canvas);
   }
}
