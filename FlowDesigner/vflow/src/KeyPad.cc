// Copyright (C) 2001 Dominic Letourneau

#include "KeyPad.h"
#include "net_types.h"
#include "Object.h"
#include <gnome.h>
#include <sstream>
#include "Vector.h"

using namespace std;

namespace FD {

DECLARE_NODE(KeyPad)
/*Node
 *
 * @name KeyPad
 * @category Probe
 * @description No description available
 *
 * @output_name KEYPAD
 * @output_type Char
 * @output_description The Char description of the key that is pressed
 *
END*/

KeyPad::KeyPad(string nodeName, ParameterSet params) 
  : BufferedNode(nodeName,params) {

  //adding outputs
    m_outputID = addOutput("KEYPAD");

  //adding parameters
   m_key = nilObject;
   m_time.tv_sec = -1;
   m_time.tv_usec = -1;
}

KeyPad::~KeyPad() {
  
  gdk_threads_enter(); 
  

  //delete main widget (window)
  if (window) {
    gtk_object_destroy(GTK_OBJECT(window));
  }

  gdk_threads_leave();
}

void KeyPad::initialize()
{

   //calling BufferedNode initialize()
   BufferedNode::initialize();


   gdk_threads_enter();


   //GTK widget creation


   window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   gtk_window_set_title (GTK_WINDOW (window), _("KeyPad"));

   table1 = gtk_table_new (1, 3, FALSE);
   gtk_widget_show (table1);
   gtk_container_add (GTK_CONTAINER (window), table1);

   table6 = gtk_table_new (6, 1, FALSE);
   gtk_widget_show (table6);
   gtk_table_attach (GTK_TABLE (table1), table6, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_A = gtk_button_new_with_mnemonic (_("     A     "));
   gtk_widget_show (button_A);
   gtk_table_attach (GTK_TABLE (table6), button_A, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_B = gtk_button_new_with_mnemonic (_("B"));
   gtk_widget_show (button_B);
   gtk_table_attach (GTK_TABLE (table6), button_B, 0, 1, 1, 2,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_C = gtk_button_new_with_mnemonic (_("C"));
   gtk_widget_show (button_C);
   gtk_table_attach (GTK_TABLE (table6), button_C, 0, 1, 2, 3,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_D = gtk_button_new_with_mnemonic (_("D"));
   gtk_widget_show (button_D);
   gtk_table_attach (GTK_TABLE (table6), button_D, 0, 1, 3, 4,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_E = gtk_button_new_with_mnemonic (_("E"));
   gtk_widget_show (button_E);
   gtk_table_attach (GTK_TABLE (table6), button_E, 0, 1, 4, 5,
                     (GtkAttachOptions) (GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_F = gtk_button_new_with_mnemonic (_("F"));
   gtk_widget_show (button_F);
   gtk_table_attach (GTK_TABLE (table6), button_F, 0, 1, 5, 6,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   table4 = gtk_table_new (2, 1, FALSE);
   gtk_widget_show (table4);
   gtk_table_attach (GTK_TABLE (table1), table4, 2, 3, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_add = gtk_button_new_with_mnemonic (_("+"));
   gtk_widget_show (button_add);
   gtk_table_attach (GTK_TABLE (table4), button_add, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_equa = gtk_button_new_with_mnemonic (_("     =     "));
   gtk_widget_show (button_equa);
   gtk_table_attach (GTK_TABLE (table4), button_equa, 0, 1, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   table2 = gtk_table_new (2, 1, FALSE);
   gtk_widget_show (table2);
   gtk_table_attach (GTK_TABLE (table1), table2, 1, 2, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   table3 = gtk_table_new (4, 3, FALSE);
   gtk_widget_show (table3);
   gtk_table_attach (GTK_TABLE (table2), table3, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_div = gtk_button_new_with_mnemonic (_("\n     /     \n"));
   gtk_widget_show (button_div);
   gtk_table_attach (GTK_TABLE (table3), button_div, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_mul = gtk_button_new_with_mnemonic (_("\n     *     \n"));
   gtk_widget_show (button_mul);
   gtk_table_attach (GTK_TABLE (table3), button_mul, 1, 2, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_sub = gtk_button_new_with_mnemonic (_("\n     -     \n"));
   gtk_widget_show (button_sub);
   gtk_table_attach (GTK_TABLE (table3), button_sub, 2, 3, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_7 = gtk_button_new_with_mnemonic (_("\n7\n"));
   gtk_widget_show (button_7);
   gtk_table_attach (GTK_TABLE (table3), button_7, 0, 1, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_8 = gtk_button_new_with_mnemonic (_("\n8\n"));
   gtk_widget_show (button_8);
   gtk_table_attach (GTK_TABLE (table3), button_8, 1, 2, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_9 = gtk_button_new_with_mnemonic (_("\n9\n"));
   gtk_widget_show (button_9);
   gtk_table_attach (GTK_TABLE (table3), button_9, 2, 3, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_4 = gtk_button_new_with_mnemonic (_("\n4\n"));
   gtk_widget_show (button_4);
   gtk_table_attach (GTK_TABLE (table3), button_4, 0, 1, 2, 3,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_5 = gtk_button_new_with_mnemonic (_("\n5\n"));
   gtk_widget_show (button_5);
   gtk_table_attach (GTK_TABLE (table3), button_5, 1, 2, 2, 3,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_6 = gtk_button_new_with_mnemonic (_("\n6\n"));
   gtk_widget_show (button_6);
   gtk_table_attach (GTK_TABLE (table3), button_6, 2, 3, 2, 3,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_1 = gtk_button_new_with_mnemonic (_("\n1\n"));
   gtk_widget_show (button_1);
   gtk_table_attach (GTK_TABLE (table3), button_1, 0, 1, 3, 4,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_2 = gtk_button_new_with_mnemonic (_("\n2\n"));
   gtk_widget_show (button_2);
   gtk_table_attach (GTK_TABLE (table3), button_2, 1, 2, 3, 4,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_3 = gtk_button_new_with_mnemonic (_("\n3\n"));
   gtk_widget_show (button_3);
   gtk_table_attach (GTK_TABLE (table3), button_3, 2, 3, 3, 4,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   table5 = gtk_table_new (1, 2, FALSE);
   gtk_widget_show (table5);
   gtk_table_attach (GTK_TABLE (table2), table5, 0, 1, 1, 2,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_0 = gtk_button_new_with_mnemonic (_("\n            0           \n"));
   gtk_widget_show (button_0);
   gtk_table_attach (GTK_TABLE (table5), button_0, 0, 1, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

   button_dot = gtk_button_new_with_mnemonic (_("\n     .      \n"));
   gtk_widget_show (button_dot);
   gtk_table_attach (GTK_TABLE (table5), button_dot, 1, 2, 0, 1,
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL),
                     (GtkAttachOptions) (GTK_EXPAND | GTK_SHRINK | GTK_FILL), 0, 0);

  g_signal_connect ((gpointer) window, "key_press_event",
                    G_CALLBACK (on_KeyPad_key_press_event),
                    this);
  g_signal_connect ((gpointer) button_A, "pressed",
                    G_CALLBACK (on_button_A_pressed),
                    this);
  g_signal_connect ((gpointer) button_A, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_B, "pressed",
                    G_CALLBACK (on_button_B_pressed),
                    this);
  g_signal_connect ((gpointer) button_B, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_C, "pressed",
                    G_CALLBACK (on_button_C_pressed),
                    this);
  g_signal_connect ((gpointer) button_C, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_D, "pressed",
                    G_CALLBACK (on_button_D_pressed),
                    this);
  g_signal_connect ((gpointer) button_D, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_E, "pressed",
                    G_CALLBACK (on_button_E_pressed),
                    this);
  g_signal_connect ((gpointer) button_E, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_F, "pressed",
                    G_CALLBACK (on_button_F_pressed),
                    this);
  g_signal_connect ((gpointer) button_F, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_0, "pressed",
                    G_CALLBACK (on_button_0_pressed),
                    this);
  g_signal_connect ((gpointer) button_0, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_1, "pressed",
                    G_CALLBACK (on_button_1_pressed),
                    this);
  g_signal_connect ((gpointer) button_1, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_2, "pressed",
                    G_CALLBACK (on_button_2_pressed),
                    this);
  g_signal_connect ((gpointer) button_2, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_3, "pressed",
                    G_CALLBACK (on_button_3_pressed),
                    this);
  g_signal_connect ((gpointer) button_3, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_4, "pressed",
                    G_CALLBACK (on_button_4_pressed),
                    this);
  g_signal_connect ((gpointer) button_4, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_5, "pressed",
                    G_CALLBACK (on_button_5_pressed),
                    this);
  g_signal_connect ((gpointer) button_5, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_6, "pressed",
                    G_CALLBACK (on_button_6_pressed),
                    this);
  g_signal_connect ((gpointer) button_6, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_7, "pressed",
                    G_CALLBACK (on_button_7_pressed),
                    this);
  g_signal_connect ((gpointer) button_7, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_8, "pressed",
                    G_CALLBACK (on_button_8_pressed),
                    this);
  g_signal_connect ((gpointer) button_8, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_9, "pressed",
                    G_CALLBACK (on_button_9_pressed),
                    this);
  g_signal_connect ((gpointer) button_9, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_dot, "pressed",
                    G_CALLBACK (on_button_dot_pressed),
                    this);
  g_signal_connect ((gpointer) button_dot, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_div, "pressed",
                    G_CALLBACK (on_button_div_pressed),
                    this);
  g_signal_connect ((gpointer) button_div, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_mul, "pressed",
                    G_CALLBACK (on_button_mul_pressed),
                    this);
  g_signal_connect ((gpointer) button_mul, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_sub, "pressed",
                    G_CALLBACK (on_button_sub_pressed),
                    this);
  g_signal_connect ((gpointer) button_sub, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_add, "pressed",
                    G_CALLBACK (on_button_add_pressed),
                    this);
  g_signal_connect ((gpointer) button_add, "released",
                    G_CALLBACK (on_button_released),
                    this);
  g_signal_connect ((gpointer) button_equa, "pressed",
                    G_CALLBACK (on_button_equa_pressed),
                    this);
  g_signal_connect ((gpointer) button_equa, "released",
                    G_CALLBACK (on_button_released),
                    this);

   //show main window
   gtk_widget_show(window);

   gdk_threads_leave(); 
}

void KeyPad::calculate(int output_id, int count, Buffer &out)
{
   if (m_time.tv_sec != -1)
   {
      struct timeval timev;
      struct timezone timez;
      gettimeofday(&timev, &timez);

      if (timev. tv_sec > m_time.tv_sec || timev.tv_usec > m_time.tv_usec)
      {
         m_key = nilObject;
         m_time.tv_sec = -1;
         m_time.tv_usec = -1;
      }
   }
   out[count] = m_key;
}

void KeyPad::reset()
{
   m_key = nilObject;
}

void KeyPad::setKeyValue(char keyValue, int timer)
{
   struct timezone timez;
   char tmp[2];
   tmp[0]=keyValue;
   tmp[1]=0;
   if (timer != -1)
   {
      gettimeofday(&m_time, &timez);
      m_time.tv_usec += timer;
   }
   else
   {
      m_time.tv_sec = -1;
      m_time.tv_usec = -1;
   }
   m_key = ObjectRef(new String(string(tmp)));
}

//callbacks
gboolean on_KeyPad_key_press_event(GtkWidget *widget,
                                 GdkEventKey *event,
                                 gpointer user_data)
{
   switch(event->keyval)
   {
   case '`': case '~': case '1': case '!': case '2': case '@':
   case '3': case '#': case '4': case '$': case '5': case '%':
   case '6': case '^': case '7': case '&': case '8': case '*':
   case '9': case '(': case '0': case ')': case '-': case '_':
   case '=': case '+': case 'q': case 'Q': case 'w': case 'W':
   case 'e': case 'E': case 'r': case 'R': case 't': case 'T':
   case 'y': case 'Y': case 'u': case 'U': case 'i': case 'I':
   case 'o': case 'O': case 'p': case 'P': case '[': case '{':
   case ']': case '}': case '\\': case '|': case 'a': case 'A':
   case 's': case 'S': case 'd': case 'D': case 'f': case 'F':
   case 'g': case 'G': case 'h': case 'H': case 'j': case 'J':
   case 'k': case 'K': case 'l': case 'L': case ';': case ':':
   case '\'': case '"': case 'z': case 'Z': case 'x': case 'X':
   case 'c': case 'C': case 'v': case 'V': case 'b': case 'B':
   case 'n': case 'N': case 'm': case 'M': case ',': case '<':
   case '.': case '>': case '/': case '?':
      ((KeyPad*)user_data)->setKeyValue(event->keyval,250000);
      break;
   default:
      break;
   }
   return 0;
}

void on_button_released(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->reset();
}

void on_button_A_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('A');
}

void on_button_B_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('B');
}

void on_button_C_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('C');
}

void on_button_D_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('D');
}

void on_button_E_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('E');
}

void on_button_F_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('F');
}

void on_button_0_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('0');
}

void on_button_1_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('1');
}

void on_button_2_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('2');
}

void on_button_3_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('3');
}

void on_button_4_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('4');
}

void on_button_5_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('5');
}

void on_button_6_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('6');
}

void on_button_7_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('7');
}

void on_button_8_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('8');
}

void on_button_9_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('9');
}

void on_button_dot_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('.');
}

void on_button_div_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('/');
}

void on_button_mul_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('*');
}

void on_button_sub_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('-');
}

void on_button_add_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('+');
}

void on_button_equa_pressed(GtkButton *button, gpointer user_data)
{
   ((KeyPad*)user_data)->setKeyValue('=');
}

}//namespace FD
