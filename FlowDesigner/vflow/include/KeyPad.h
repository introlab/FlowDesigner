#ifndef KEYPAD_H
#define KEYPAD_H

#include "Probe.h"
#include <map>

using namespace std;

class KeyPad : public Node {

  //callbacks
  friend void keypad_button_pressed(GtkButton  *object, KeyPad* keypad);
  friend void keypad_button_released(GtkButton  *button, KeyPad *keypad);
  friend gboolean ignore_delete(GtkWidget *widget, GdkEvent *event, KeyPad *keypad);
  friend gboolean keypad_event_function  (GtkWidget *window, GdkEvent *event, KeyPad *keypad);
  protected:

  //updated the pad
  void update_values(int pad_number, bool activate);
  void add_accelerator(char key, GtkWidget *button);
  void keyboard_action(unsigned int key);
  
  //widgets
  GtkWidget *window1;
  GtkWidget *vbox1;
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

  //Accelerator
  GtkAccelGroup *accel;

  //internal variables
  int selected_column;
  int selected_line;
  int selected_pad;
  char pad_description;
  bool active;
  int key_stroke;

  //outputs
  int keypadID;
  int keypadIdID;
  int keypadNameID;

  //a lookup map with widgets/ids
  map <GtkWidget*,int> button_map;
  map <char,int> key_map;

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
