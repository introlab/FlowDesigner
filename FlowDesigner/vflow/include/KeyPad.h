// Copyright (C) 2004 Dominic Letourneau

#ifndef _KEYPAD_H
#define _KEYPAD_H

#include "Probe.h"
#include "BufferedNode.h"
#include <map>

class KeyPad : public BufferedNode {


  //callbacks
  friend void on_button_7_clicked (GtkButton       *button, KeyPad*         user_data);

  friend void on_button_8_clicked (GtkButton       *button, KeyPad*         user_data);

  friend void on_button_9_clicked (GtkButton       *button, KeyPad*         user_data);

  friend void on_button_A_clicked (GtkButton       *button, KeyPad*         user_data);

  friend void on_button_4_clicked (GtkButton       *button, KeyPad*         user_data);

  friend void on_button_5_clicked (GtkButton       *button, KeyPad*         user_data);

  friend void on_button_6_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_B_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_1_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_2_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_3_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_C_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_0_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_F_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_E_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);

  friend void on_button_D_clicked                    (GtkButton       *button,
						      KeyPad*         user_data);
 private: 

  //inputs & outputs
  int m_outputID;
 
  
  //widgets
  GtkWidget *window1;
   GtkWidget *table1;
   GtkWidget *button_7;
   GtkWidget *button_8;
   GtkWidget *button_9;
   GtkWidget *button_A;
   GtkWidget *button_4;
   GtkWidget *button_5;
   GtkWidget *button_6;
   GtkWidget *button_B;
   GtkWidget *button_1;
   GtkWidget *button_2;
   GtkWidget *button_3;
   GtkWidget *button_C;
   GtkWidget *button_0;
   GtkWidget *button_F;
   GtkWidget *button_E;
   GtkWidget *button_D;

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
