// Copyright (C) 2001 Jean-Marc Valin

#ifndef PLOTPROBE_H
#define PLOTPROBE_H

#include "Probe.h"

namespace FD {

class PlotProbe : public Probe {
  protected:
   GnomeCanvas *canvas;
   GnomeCanvasGroup *group;
   
   GnomeCanvasItem *item;

   double xmin, xmax, ymin, ymax;

  public:

   PlotProbe(std::string nodeName, ParameterSet params);

   virtual ~PlotProbe();

   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   //virtual ObjectRef getOutput(int output_id, int count); 

   virtual void display();

   virtual void show_hide();

};

}//namespace FD

#endif
