// Copyright (C) 2001 Dominic Letourneau

#include "KeyPad.h"
#include "net_types.h"
#include "Object.h"
#include <gnome.h>
#include <sstream>
#include "Vector.h"

DECLARE_NODE(KeyPad)
/*Node
 *
 * @name KeyPad
 * @category Probe
 * @description No description available
 *
 * @output_name KEYPAD
 * @output_type Vector<int>
 * @output_description A vector of size 2 representing the tuple Line/column of the pressed key.
 *
 *
END*/

KeyPad::KeyPad(string nodeName, ParameterSet params) 
  : BufferedNode(nodeName,params) {

  //adding outputs
    m_outputID = addOutput("KEYPAD");

  //adding parameters

}

KeyPad::~KeyPad() {
  
  gdk_threads_enter(); 
  

  //delete main widget (window)
  if (window1) {
    gtk_object_destroy(GTK_OBJECT(window1));
  }
  
  gdk_threads_leave(); 
}

void KeyPad::initialize() {
 
   //calling BufferedNode initialize()
   BufferedNode::initialize();


   gdk_threads_enter(); 


   //GTK widget creation

   
   window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   gtk_window_set_title (GTK_WINDOW (window1), _("window1"));
   
   table1 = gtk_table_new (4, 4, TRUE);
   gtk_widget_show (table1);
   gtk_container_add (GTK_CONTAINER (window1), table1);
   gtk_table_set_row_spacings (GTK_TABLE (table1), 1);
   gtk_table_set_col_spacings (GTK_TABLE (table1), 1);
   
   button_7 = gtk_button_new_with_mnemonic (_("7"));
   gtk_widget_show (button_7);
   gtk_table_attach (GTK_TABLE (table1), button_7, 0, 1, 0, 1,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_8 = gtk_button_new_with_mnemonic (_("8"));
   gtk_widget_show (button_8);
   gtk_table_attach (GTK_TABLE (table1), button_8, 1, 2, 0, 1,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_9 = gtk_button_new_with_mnemonic (_("9"));
   gtk_widget_show (button_9);
   gtk_table_attach (GTK_TABLE (table1), button_9, 2, 3, 0, 1,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_A = gtk_button_new_with_mnemonic (_("A"));
   gtk_widget_show (button_A);
   gtk_table_attach (GTK_TABLE (table1), button_A, 3, 4, 0, 1,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_4 = gtk_button_new_with_mnemonic (_("4"));
   gtk_widget_show (button_4);
   gtk_table_attach (GTK_TABLE (table1), button_4, 0, 1, 1, 2,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_5 = gtk_button_new_with_mnemonic (_("5"));
   gtk_widget_show (button_5);
   gtk_table_attach (GTK_TABLE (table1), button_5, 1, 2, 1, 2,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_6 = gtk_button_new_with_mnemonic (_("6"));
   gtk_widget_show (button_6);
   gtk_table_attach (GTK_TABLE (table1), button_6, 2, 3, 1, 2,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_B = gtk_button_new_with_mnemonic (_("B"));
   gtk_widget_show (button_B);
   gtk_table_attach (GTK_TABLE (table1), button_B, 3, 4, 1, 2,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_1 = gtk_button_new_with_mnemonic (_("1"));
   gtk_widget_show (button_1);
   gtk_table_attach (GTK_TABLE (table1), button_1, 0, 1, 2, 3,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_2 = gtk_button_new_with_mnemonic (_("2"));
   gtk_widget_show (button_2);
   gtk_table_attach (GTK_TABLE (table1), button_2, 1, 2, 2, 3,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

   button_3 = gtk_button_new_with_mnemonic (_("3"));
   gtk_widget_show (button_3);
   gtk_table_attach (GTK_TABLE (table1), button_3, 2, 3, 2, 3,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_C = gtk_button_new_with_mnemonic (_("C"));
   gtk_widget_show (button_C);
   gtk_table_attach (GTK_TABLE (table1), button_C, 3, 4, 2, 3,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_0 = gtk_button_new_with_mnemonic (_("0"));
   gtk_widget_show (button_0);
   gtk_table_attach (GTK_TABLE (table1), button_0, 0, 1, 3, 4,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_F = gtk_button_new_with_mnemonic (_("F"));
   gtk_widget_show (button_F);
   gtk_table_attach (GTK_TABLE (table1), button_F, 1, 2, 3, 4,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_E = gtk_button_new_with_mnemonic (_("E"));
   gtk_widget_show (button_E);
   gtk_table_attach (GTK_TABLE (table1), button_E, 2, 3, 3, 4,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
   
   button_D = gtk_button_new_with_mnemonic (_("D"));
   gtk_widget_show (button_D);
   gtk_table_attach (GTK_TABLE (table1), button_D, 3, 4, 3, 4,
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		     (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);

   g_signal_connect ((gpointer) button_7, "clicked",
		     G_CALLBACK (on_button_7_clicked),
		     this);
   g_signal_connect ((gpointer) button_8, "clicked",
		     G_CALLBACK (on_button_8_clicked),
		     this);
   g_signal_connect ((gpointer) button_9, "clicked",
		     G_CALLBACK (on_button_9_clicked),
		     this);
   g_signal_connect ((gpointer) button_A, "clicked",
		     G_CALLBACK (on_button_A_clicked),
		     this);
   g_signal_connect ((gpointer) button_4, "clicked",
		     G_CALLBACK (on_button_4_clicked),
		     this);
   g_signal_connect ((gpointer) button_5, "clicked",
		     G_CALLBACK (on_button_5_clicked),
		     this);
   g_signal_connect ((gpointer) button_6, "clicked",
		     G_CALLBACK (on_button_6_clicked),
		     this);
   g_signal_connect ((gpointer) button_B, "clicked",
		     G_CALLBACK (on_button_B_clicked),
		     this);
   g_signal_connect ((gpointer) button_1, "clicked",
		     G_CALLBACK (on_button_1_clicked),
		     this);
   g_signal_connect ((gpointer) button_2, "clicked",
		     G_CALLBACK (on_button_2_clicked),
		     this);
   g_signal_connect ((gpointer) button_3, "clicked",
		     G_CALLBACK (on_button_3_clicked),
		     this);
   g_signal_connect ((gpointer) button_C, "clicked",
		     G_CALLBACK (on_button_C_clicked),
		     this);
   g_signal_connect ((gpointer) button_0, "clicked",
		     G_CALLBACK (on_button_0_clicked),
		     this);
   g_signal_connect ((gpointer) button_F, "clicked",
		     G_CALLBACK (on_button_F_clicked),
		     this);
   g_signal_connect ((gpointer) button_E, "clicked",
		     G_CALLBACK (on_button_E_clicked),
		     this);
   g_signal_connect ((gpointer) button_D, "clicked",
		     G_CALLBACK (on_button_D_clicked),
		     this);

   /* Store pointers to all widgets, for use by lookup_widget(). */
   /*
   GLADE_HOOKUP_OBJECT_NO_REF (window1, window1, "window1");
   GLADE_HOOKUP_OBJECT (window1, table1, "table1");
   GLADE_HOOKUP_OBJECT (window1, button_7, "button_7");
   GLADE_HOOKUP_OBJECT (window1, button_8, "button_8");
   GLADE_HOOKUP_OBJECT (window1, button_9, "button_9");
   GLADE_HOOKUP_OBJECT (window1, button_A, "button_A");
   GLADE_HOOKUP_OBJECT (window1, button_4, "button_4");
   GLADE_HOOKUP_OBJECT (window1, button_5, "button_5");
   GLADE_HOOKUP_OBJECT (window1, button_6, "button_6");
   GLADE_HOOKUP_OBJECT (window1, button_B, "button_B");
   GLADE_HOOKUP_OBJECT (window1, button_1, "button_1");
   GLADE_HOOKUP_OBJECT (window1, button_2, "button_2");
   GLADE_HOOKUP_OBJECT (window1, button_3, "button_3");
   GLADE_HOOKUP_OBJECT (window1, button_C, "button_C");
   GLADE_HOOKUP_OBJECT (window1, button_0, "button_0");
   GLADE_HOOKUP_OBJECT (window1, button_F, "button_F");
   GLADE_HOOKUP_OBJECT (window1, button_E, "button_E");
   GLADE_HOOKUP_OBJECT (window1, button_D, "button_D");
   */

   gtk_widget_grab_focus (table1);

   //show main window
   gtk_widget_show(window1);
   
   gdk_threads_leave(); 
}

void KeyPad::reset() {

  //reset stuff if needed

}



void KeyPad::calculate(int output_id, int count, Buffer &out) {

  out[count] = nilObject;

}


//callbacks
void
on_button_7_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_8_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_9_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_A_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_4_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_5_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_6_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_B_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_1_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_2_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_3_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_C_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_0_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_F_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_E_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}


void
on_button_D_clicked                    (GtkButton       *button,
                                        KeyPad*         user_data)
{

}

