#ifndef KEYPAD_H
#define KEYPAD_H

#include "Probe.h"

class KeyPad : public Probe {

  protected:

  GtkWidget *table_1;

  GtkWidget *button_1;
  GtkWidget *button_2;
  GtkWidget *button_3;
  GtkWidget *button_4;
  GtkWidget *button_5;
  GtkWidget *button_6;
  GtkWidget *button_7;
  GtkWidget *button_8;
  GtkWidget *button_9;
  GtkWidget *button_0;
  GtkWidget *button_star;
  GtkWidget *button_hash;
  GtkWidget *button_A;
  GtkWidget *button_B;
  GtkWidget *button_C;
  GtkWidget *button_D;

  int column;
  int line;
  bool changed;

  int keypadID;

  public:

   KeyPad(string nodeName, ParameterSet params);

   virtual ~KeyPad();

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */

   virtual ObjectRef getOutput(int output_id, int count); 

   virtual void display();

};


#endif
