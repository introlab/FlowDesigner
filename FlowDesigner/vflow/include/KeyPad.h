// Copyright (C) 2004 Dominic Letourneau

#ifndef _KEYPAD_H
#define _KEYPAD_H

#include "Probe.h"
#include "BufferedNode.h"
#include <map>

class KeyPad : public BufferedNode {

 private:
  //callbacks
 

  //updated the pad
 
  
  //widgets
 
  public:

   KeyPad(string nodeName, ParameterSet params);

   virtual ~KeyPad();

   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

 
   void calculate(int output_id, int count, Buffer &out);
   
 
};


#endif
