// Copyright (C) 2001 Jean-Marc Valin

#ifndef TEXTPROBE_H
#define TEXTPROBE_H

#include "Probe.h"

namespace FD {

class TextProbe : public Probe {
  protected:
   GtkWidget *less1;
  public:

   TextProbe(std::string nodeName, ParameterSet params);

   virtual ~TextProbe();

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
};

}//namespace FD
#endif
