// Copyright (C) 2001 Jean-Marc Valin

#ifndef GTKPLOTPROBE_H
#define GTKPLOTPROBE_H

#include "Probe.h"
#include "gtkextra/gtkextra.h"

class GtkPlotProbe : public Probe {
  protected:
   GtkWidget *canvas;
   GtkWidget *active_plot;
   GtkPlotData *dataset;
   vector<gdouble> x;
   vector<gdouble> y;
   vector<gdouble> dx;
   vector<gdouble> dy;
   double xmin, xmax, ymin, ymax;

  public:

   GtkPlotProbe(string nodeName, ParameterSet params);

   virtual ~GtkPlotProbe();

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   //virtual ObjectRef getOutput(int output_id, int count); 

   virtual void display();

   virtual void show_hide();

};


#endif
