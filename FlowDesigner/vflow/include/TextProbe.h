// Copyright (C) 2001 Jean-Marc Valin

#ifndef TEXTPROBE_H
#define TEXTPROBE_H

#include "Probe.h"

class TextProbe : public Probe {
  protected:
   GtkWidget *less1;
  public:

   TextProbe(string nodeName, ParameterSet params);

   virtual ~TextProbe();

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
};


#endif
