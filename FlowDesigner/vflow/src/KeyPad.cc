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
 * @output_name KEYPAD_ID
 * @output_type int
 * @output_description The Id of the key that is pressed
 *
 * @output_name KEYPAD_NAME
 * @output_type Char
 * @output_description The Char description of the key that is pressed
 *
 * @output_name ACTIVATED
 * @output_type bool
 * @output_description True if the user is pressing a button, else false.
 *
END*/

  

const int KeyPad::pad_0_number = 0;
const int KeyPad::pad_1_number = 1;
const int KeyPad::pad_2_number = 2;
const int KeyPad::pad_3_number = 3;
const int KeyPad::pad_4_number = 4;
const int KeyPad::pad_5_number = 5;
const int KeyPad::pad_6_number = 6;
const int KeyPad::pad_7_number = 7;
const int KeyPad::pad_8_number = 8;
const int KeyPad::pad_9_number = 9;
const int KeyPad::pad_star_number = 10;
const int KeyPad::pad_hash_number = 11;
const int KeyPad::pad_A_number = 12;
const int KeyPad::pad_B_number = 13;
const int KeyPad::pad_C_number = 14;
const int KeyPad::pad_D_number =15;

KeyPad::KeyPad(string nodeName, ParameterSet params) 
  : Node(nodeName,params), active(false), selected_line(-1), 
  selected_column(-1), selected_pad(-1), key_stroke(0) {

  //adding outputs
  keypadID = addOutput("KEYPAD");
  keypadIdID = addOutput("KEYPAD_ID");
  keypadNameID = addOutput("KEYPAD_NAME");
  keypadActivatedID = addOutput("ACTIVATED");

}

KeyPad::~KeyPad() {
  
  gdk_threads_enter(); 
  
  if (window1) {
    gtk_widget_destroy (window1);
  }
  
  gdk_threads_leave(); 
}

void KeyPad::specificInitialize() {
 
  //calling Node specificInitialize()
   this->Node::specificInitialize();


   gdk_threads_enter(); 

   //creating accelerator
   accel = gtk_accel_group_new();
   

   //creating window
   window1 = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   gtk_object_set_data (GTK_OBJECT (window1), "window1", window1);

   gtk_window_set_title (GTK_WINDOW (window1), "KeyPad");

   gtk_signal_connect (GTK_OBJECT (window1), "delete-event",
		       GTK_SIGNAL_FUNC (ignore_delete),this);

   gtk_window_set_default_size (GTK_WINDOW(window1),280,150);
   //creating vbox
   vbox1 = gtk_vbox_new (FALSE, 0);
   gtk_widget_ref (vbox1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "vbox1", vbox1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (vbox1);
   gtk_container_add (GTK_CONTAINER (window1), vbox1);




   //creating table_1
   table_1 = gtk_table_new (4,4,TRUE);
   gtk_widget_ref (table_1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "table_1", table_1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (table_1);
   

   //creating button1
   button_1 = gtk_button_new_with_label ("1");
   button_map.insert(make_pair(button_1,pad_1_number));
   key_map.insert(make_pair('1',pad_1_number));
   gtk_widget_ref (button_1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_1", button_1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_1);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_1,0,1,0,1);
  
   
   //creating button2
   button_2 = gtk_button_new_with_label ("2");
   button_map.insert(make_pair(button_2,pad_2_number));
   key_map.insert(make_pair('2',pad_2_number));
   gtk_widget_ref (button_2);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_2", button_2,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_2);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_2,1,2,0,1);

   //creating button3
   button_3 = gtk_button_new_with_label ("3");
   button_map.insert(make_pair(button_3,pad_3_number));
   key_map.insert(make_pair('3',pad_3_number));
   gtk_widget_ref (button_3);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_3", button_3,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_3);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_3,2,3,0,1);


   //creating button4
   button_4 = gtk_button_new_with_label ("4");
   button_map.insert(make_pair(button_4,pad_4_number));
   key_map.insert(make_pair('4',pad_4_number));
   gtk_widget_ref (button_4);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_4", button_4,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_4);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_4,0,1,1,2);


   //creating button5
   button_5 = gtk_button_new_with_label ("5");
   button_map.insert(make_pair(button_5,pad_5_number));
   key_map.insert(make_pair('5',pad_5_number));
   gtk_widget_ref (button_5);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_5", button_5,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_5);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_5,1,2,1,2);


   //creating button6
   button_6 = gtk_button_new_with_label ("6");
   button_map.insert(make_pair(button_6,pad_6_number));
   key_map.insert(make_pair('6',pad_6_number));
   gtk_widget_ref (button_6);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_6", button_6,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_6);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_6,2,3,1,2);


   //creating button7
   button_7 = gtk_button_new_with_label ("7");
   button_map.insert(make_pair(button_7,pad_7_number));
   key_map.insert(make_pair('7',pad_7_number));
   gtk_widget_ref (button_7);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_7", button_7,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_7);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_7,0,1,2,3);


   //creating button8
   button_8 = gtk_button_new_with_label ("8");
   button_map.insert(make_pair(button_8,pad_8_number));
   key_map.insert(make_pair('8',pad_8_number));
   gtk_widget_ref (button_8);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_8", button_8,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_8);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_8,1,2,2,3);


   //creating button9
   button_9 = gtk_button_new_with_label ("9");
   button_map.insert(make_pair(button_9,pad_9_number));
   key_map.insert(make_pair('9',pad_9_number));
   gtk_widget_ref (button_9);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_9", button_9,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_9);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_9,2,3,2,3);


   //creating button0
   button_0 = gtk_button_new_with_label ("0");
   button_map.insert(make_pair(button_0,pad_0_number));
   key_map.insert(make_pair('0',pad_0_number));
   gtk_widget_ref (button_0);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_0", button_0,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_0);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_0,1,2,3,4);


   //creating button_hash
   button_hash = gtk_button_new_with_label ("#");
   button_map.insert(make_pair(button_hash,pad_hash_number));
   key_map.insert(make_pair('#',pad_hash_number));
   gtk_widget_ref (button_hash);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_hash", button_hash,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_hash);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_hash,2,3,3,4);


   //creating button_star
   button_star = gtk_button_new_with_label ("*");
   button_map.insert(make_pair(button_star,pad_star_number));
   key_map.insert(make_pair('*',pad_star_number));
   gtk_widget_ref (button_star);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_star", button_star,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_star);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_star,0,1,3,4);


   //creating button_A
   button_A = gtk_button_new_with_label ("A");
   button_map.insert(make_pair(button_A,pad_A_number));
   key_map.insert(make_pair('A',pad_A_number));
   gtk_widget_ref (button_A);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_A", button_A,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_A);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_A,3,4,0,1);

   //creating button_B
   button_B = gtk_button_new_with_label ("B");
   button_map.insert(make_pair(button_B,pad_B_number));
   key_map.insert(make_pair('B',pad_B_number));
   gtk_widget_ref (button_B);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_B", button_B,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_B);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_B,3,4,1,2);
   
   //creating button_C
   button_C = gtk_button_new_with_label ("C");
   button_map.insert(make_pair(button_C,pad_C_number));
   key_map.insert(make_pair('C',pad_C_number));
   gtk_widget_ref (button_C);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_C", button_C,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_C);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_C,3,4,2,3);

   //creating button_D
   button_D = gtk_button_new_with_label ("D");
   button_map.insert(make_pair(button_D,pad_D_number));
   key_map.insert(make_pair('D',pad_D_number));
   gtk_widget_ref (button_D);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_D", button_D,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_D);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_D,3,4,3,4);

   //packing table
   gtk_box_pack_start (GTK_BOX (vbox1), table_1, TRUE, TRUE, 0);


   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_1), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_2), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_3), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_4), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_5), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_6), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_7), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_8), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);


   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_9), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_0), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_hash), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_star), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);


   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_A), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_B), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_C), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_D), "pressed",
		       GTK_SIGNAL_FUNC (keypad_button_pressed),
		       this);
   


   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_1), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_2), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_3), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_4), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_5), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_6), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_7), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_8), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);


   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_9), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_0), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_hash), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_star), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);


   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_A), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_B), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);
   

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_C), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);

   //connecting signals
   gtk_signal_connect (GTK_OBJECT (button_D), "released",
		       GTK_SIGNAL_FUNC (keypad_button_released),
		       this);



   gtk_signal_connect(GTK_OBJECT(window1), "event",
		      GTK_SIGNAL_FUNC (keypad_event_function),this);


   //adding numeric keys
   key_map.insert(make_pair(65456,pad_0_number));
   key_map.insert(make_pair(65457,pad_1_number));
   key_map.insert(make_pair(65458,pad_2_number));
   key_map.insert(make_pair(65459,pad_3_number));
   key_map.insert(make_pair(65460,pad_4_number));
   key_map.insert(make_pair(65461,pad_5_number));
   key_map.insert(make_pair(65462,pad_6_number));
   key_map.insert(make_pair(65463,pad_7_number));
   key_map.insert(make_pair(65464,pad_8_number));
   key_map.insert(make_pair(65465,pad_9_number));
   key_map.insert(make_pair(65450,pad_star_number));

   //let's show window
   gtk_accel_group_attach(accel,GTK_OBJECT(window1));
   gtk_widget_show(window1);

   gdk_threads_leave(); 
}

void KeyPad::reset() {

  active = false;

}

ObjectRef KeyPad::getOutput(int output_id, int count) {

  static bool is_active = false;

  key_stroke = max(0,key_stroke - 1);

  //KeypadID
  if (output_id == keypadID) {

    if (is_active) {
      Vector<int> *my_output = new Vector<int>(2);
      
      (*my_output)[0] = selected_line;
      (*my_output)[1] = selected_column;
      return ObjectRef(my_output);
    }
    else {
      return nilObject;
    }
    
    
  }//keypadID
  else if (output_id == keypadIdID) {
    if (is_active) {
      return ObjectRef(Int::alloc(selected_pad));
    }
    else {
      return nilObject;
    }

  }//keypadNameID
  else if (output_id == keypadNameID) {
    if (is_active) {
       char tmp[2];
       tmp[0]=pad_description;
       tmp[1]=0;
      return ObjectRef(new String(string(tmp)));
    }
    else {
      return nilObject;
    }
  }//keypadActivatedID
  else if (output_id == keypadActivatedID) {
    
    if (key_stroke > 0 || active) {
      is_active = true;
    }
    else {
      is_active = false;
    }

    if (is_active) {
      return ObjectRef(Bool::alloc(true));
    }
    else {
      return ObjectRef(Bool::alloc(false));
    }
  }
  else {
    throw new NodeException (this, "KeyPad: Unknown output id", __FILE__, __LINE__);
  }
}

void KeyPad::update_values(int pad_number, bool activate) {

  //updating pad, line and column
  selected_pad = pad_number;

  switch(pad_number) {
  case pad_0_number:
    selected_line = 3;
    selected_column = 1;
    pad_description = '0';
    break;
  case pad_1_number:
    selected_line = 0;
    selected_column = 0;
    pad_description = '1';
    break;
  case pad_2_number:
    selected_line = 0;
    selected_column = 1;
    pad_description = '2';
    break;
  case pad_3_number:
    selected_line = 0;
    selected_column = 2;
    pad_description = '3';
    break;
  case pad_4_number:
    selected_line = 1;
    selected_column = 0;
    pad_description = '4';
    break;
  case pad_5_number:
    selected_line = 1;
    selected_column = 1;
    pad_description = '5';
    break;
  case pad_6_number:
    selected_line = 1;
    selected_column = 2;
    pad_description = '6';
    break;
  case pad_7_number:
    selected_line = 2;
    selected_column = 0;
    pad_description = '7';
    break;
  case pad_8_number:
    selected_line = 2;
    selected_column = 1;
    pad_description = '8';
    break;
  case pad_9_number:
    selected_line = 2;
    selected_column = 2;
    pad_description = '9';
    break;
  case pad_star_number:
    selected_line = 3;
    selected_column = 0;
    pad_description = '*';
    break;
  case pad_hash_number:
    selected_line = 3;
    selected_column = 2;
    pad_description = '#';
    break;
  case pad_A_number:
    selected_line = 0;
    selected_column = 3;
    pad_description = 'A';
    break;
  case pad_B_number:
    selected_line = 1;
    selected_column = 3;
    pad_description = 'B';
    break;
  case pad_C_number:
    selected_line = 2;
    selected_column = 3;
    pad_description = 'C';
    break;
  case pad_D_number:
    selected_line = 3;
    selected_column = 3;
    pad_description = 'D';
    break;
    
  default:
    throw new NodeException (this, "KeyPad: Invalid pad number", __FILE__, __LINE__);
    break;

  }

  
  active = activate;


}

void KeyPad::add_accelerator(char key, GtkWidget *button) {

  gtk_widget_add_accelerator (button, "pressed", accel,
                              GDK_A, 0,
                              GTK_ACCEL_VISIBLE);
}

void KeyPad::keyboard_action(unsigned int key) {
  
  if (key_map.find(key) != key_map.end()) {
    //we found this key
    update_values(key_map[key],false);
    key_stroke+= 5;
  }

}

void keypad_button_pressed(GtkButton  *button, KeyPad *keypad) {

  try {
  
    int pad_number = keypad->button_map[GTK_WIDGET(button)];
    keypad->update_values(pad_number,true);
    
  }
  catch (BaseException *e) {
    e->print(cerr);
    delete e;
  }
  catch (...) {
    cerr<<"Unknown exception occured in KeyPad.cc"<<endl;
    exit(-1);
  }
}


void keypad_button_released(GtkButton  *button, KeyPad *keypad) {
  keypad->active = false;
}


gboolean ignore_delete(GtkWidget *widget, GdkEvent *event, KeyPad *keypad) {
   return TRUE;
}

gboolean keypad_event_function  (GtkWidget *window, GdkEvent *event, KeyPad *keypad) {
  
  switch (event->type) {
  case GDK_KEY_PRESS:
    keypad->keyboard_action(event->key.keyval);
    break;
  case GDK_KEY_RELEASE:
    keypad->key_stroke = 0;
    break;
  }

  return TRUE;
}
