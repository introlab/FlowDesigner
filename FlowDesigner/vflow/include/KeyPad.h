// Copyright (C) 2004 Dominic Letourneau

#ifndef _KEYPAD_H
#define _KEYPAD_H

#include "Probe.h"
#include "BufferedNode.h"
#include <map>
#include <sys/time.h>

class KeyPad : public BufferedNode
{


  //callbacks
   friend gboolean on_KeyPad_key_press_event(GtkWidget *widget,
                                             GdkEventKey *event,
                                             gpointer user_data);

   friend void on_button_A_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_released(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_B_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_C_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_D_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_E_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_F_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_0_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_1_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_2_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_3_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_4_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_5_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_6_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_7_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_8_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_9_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_dot_pressed(GtkButton *button,
                                 gpointer user_data);

   friend void on_button_div_pressed(GtkButton *button,
                                    gpointer user_data);

   friend void on_button_mul_pressed(GtkButton *button,
                                    gpointer user_data);

   friend void on_button_sub_pressed(GtkButton *button,
                                    gpointer user_data);

   friend void on_button_add_pressed(GtkButton *button,
                                    gpointer user_data);

   friend void on_button_equa_pressed(GtkButton *button,
                                    gpointer user_data);

private:

   //inputs & outputs
   int m_outputID;


   //widgets
   GtkWidget *window;
   GtkWidget *table1;
   GtkWidget *table2;
   GtkWidget *table3;
   GtkWidget *table4;
   GtkWidget *table5;
   GtkWidget *table6;
   GtkWidget *button_A;
   GtkWidget *button_B;
   GtkWidget *button_C;
   GtkWidget *button_D;
   GtkWidget *button_E;
   GtkWidget *button_F;
   GtkWidget *button_0;
   GtkWidget *button_1;
   GtkWidget *button_2;
   GtkWidget *button_3;
   GtkWidget *button_4;
   GtkWidget *button_5;
   GtkWidget *button_6;
   GtkWidget *button_7;
   GtkWidget *button_8;
   GtkWidget *button_9;
   GtkWidget *button_dot;
   GtkWidget *button_div;
   GtkWidget *button_mul;
   GtkWidget *button_sub;
   GtkWidget *button_add;
   GtkWidget *button_equa;

   ObjectRef m_key;
   timeval m_time;

public:

   KeyPad(std::string nodeName, ParameterSet params);

   virtual ~KeyPad();

   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize();

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset();


   void calculate(int output_id, int count, Buffer &out);

   void setKeyValue(char keyValue, int timer = -1);
};


#endif
