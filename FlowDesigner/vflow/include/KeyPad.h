#ifndef KEYPAD_H
#define KEYPAD_H

#include "Probe.h"
#include <map>

using namespace std;

class KeyPad : public Probe {

  //callbacks
  friend void keypad_button_pressed(GtkButton  *object, KeyPad* keypad);
  friend void keypad_button_released(GtkButton  *button, KeyPad *keypad);
  
  protected:


  void update_values(int pad_number);

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

  int selected_column;
  int selected_line;
  int selected_pad;
  int hold_value;
  int last_update;
  int current_count;

  bool changed;

  int keypadID;

  map <GtkWidget*,int> key_map;

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

   static const int pad_0_number;
   static const int pad_1_number;
   static const int pad_2_number;
   static const int pad_3_number;
   static const int pad_4_number;
   static const int pad_5_number;
   static const int pad_6_number;
   static const int pad_7_number;
   static const int pad_8_number;
   static const int pad_9_number;
   static const int pad_star_number;
   static const int pad_hash_number;
   static const int pad_A_number;
   static const int pad_B_number;
   static const int pad_C_number;
   static const int pad_D_number;
};


#endif
