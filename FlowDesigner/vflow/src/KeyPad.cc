// Copyright (C) 2000 Dominic Letourneau
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @output_name KEYPAD
 * @output_description No description available
 *
 * @input_name INPUT
 * @output_description No description available
 *
 * @parameter_name BREAK_AT
 * @parameter_description No description available
 *
 * @parameter_name SHOW
 * @parameter_description No description available
 *
 * @parameter_name SKIP
 * @parameter_description No description available
 *
 * @parameter_name HOLD
 * @parameter_description Hold for X iteration (0 = forever)
 * @parameter_type int
 *
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
   : Probe(nodeName, params), changed(false), hold_value(0), 
  selected_line(-1), selected_column(-1), selected_pad(-1) {

  cerr<<"KeyPad Constructor"<<endl;
  keypadID = addOutput("KEYPAD");

  if (parameters.exist("HOLD")) {
    hold_value = dereference_cast<int> (parameters.get("HOLD"));
  }

  cerr<<"End Keypad Constructor"<<endl;
}

KeyPad::~KeyPad() {
  cerr<<"KeyPad Destructor"<<endl;
}

void KeyPad::specificInitialize() {

  cerr<<"Probe specific initialize"<<endl;
  Probe::specificInitialize();


  cerr<<"Keypad specific initialize"<<endl;
   NO_CANCEL;
   gdk_threads_enter(); 

   //creating table_1
   table_1 = gtk_table_new (4,4,TRUE);
   gtk_widget_ref (table_1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "table_1", table_1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (table_1);

   //creating button1
   button_1 = gtk_button_new_with_label ("1");
   key_map.insert(make_pair(button_1,pad_1_number));
   gtk_widget_ref (button_1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_1", button_1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_1);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_1,0,1,0,1);
   
   //creating button2
   button_2 = gtk_button_new_with_label ("2");
   key_map.insert(make_pair(button_2,pad_2_number));
   gtk_widget_ref (button_2);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_2", button_2,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_2);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_2,1,2,0,1);

   //creating button3
   button_3 = gtk_button_new_with_label ("3");
   key_map.insert(make_pair(button_3,pad_3_number));
   gtk_widget_ref (button_3);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_3", button_3,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_3);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_3,2,3,0,1);


   //creating button4
   button_4 = gtk_button_new_with_label ("4");
   key_map.insert(make_pair(button_4,pad_4_number));
   gtk_widget_ref (button_4);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_4", button_4,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_4);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_4,0,1,1,2);


   //creating button5
   button_5 = gtk_button_new_with_label ("5");
   key_map.insert(make_pair(button_5,pad_5_number));
   gtk_widget_ref (button_5);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_5", button_5,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_5);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_5,1,2,1,2);


   //creating button6
   button_6 = gtk_button_new_with_label ("6");
   key_map.insert(make_pair(button_6,pad_6_number));
   gtk_widget_ref (button_6);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_6", button_6,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_6);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_6,2,3,1,2);


   //creating button7
   button_7 = gtk_button_new_with_label ("7");
   key_map.insert(make_pair(button_7,pad_7_number));
   gtk_widget_ref (button_7);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_7", button_7,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_7);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_7,0,1,2,3);


   //creating button8
   button_8 = gtk_button_new_with_label ("8");
   key_map.insert(make_pair(button_8,pad_8_number));
   gtk_widget_ref (button_8);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_8", button_8,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_8);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_8,1,2,2,3);


   //creating button9
   button_9 = gtk_button_new_with_label ("9");
   key_map.insert(make_pair(button_9,pad_9_number));
   gtk_widget_ref (button_9);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_9", button_9,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_9);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_9,2,3,2,3);


   //creating button0
   button_0 = gtk_button_new_with_label ("0");
   key_map.insert(make_pair(button_0,pad_0_number));
   gtk_widget_ref (button_0);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_0", button_0,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_0);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_0,1,2,3,4);


   //creating button_hash
   button_hash = gtk_button_new_with_label ("#");
   key_map.insert(make_pair(button_hash,pad_hash_number));
   gtk_widget_ref (button_hash);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_hash", button_hash,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_hash);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_hash,2,3,3,4);


   //creating button_star
   button_star = gtk_button_new_with_label ("*");
   key_map.insert(make_pair(button_star,pad_star_number));
   gtk_widget_ref (button_star);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_star", button_star,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_star);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_star,0,1,3,4);


   //creating button_A
   button_A = gtk_button_new_with_label ("A");
   key_map.insert(make_pair(button_A,pad_A_number));
   gtk_widget_ref (button_A);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_A", button_A,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_A);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_A,3,4,0,1);

   //creating button_B
   button_B = gtk_button_new_with_label ("B");
   key_map.insert(make_pair(button_B,pad_B_number));
   gtk_widget_ref (button_B);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_B", button_B,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_B);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_B,3,4,1,2);
   
   //creating button_C
   button_C = gtk_button_new_with_label ("C");
   key_map.insert(make_pair(button_C,pad_C_number));
   gtk_widget_ref (button_C);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_C", button_C,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_C);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_C,3,4,2,3);

   //creating button_D
   button_D = gtk_button_new_with_label ("D");
   key_map.insert(make_pair(button_D,pad_D_number));
   gtk_widget_ref (button_D);
   gtk_object_set_data_full (GTK_OBJECT (window1), "button_D", button_D,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (button_D);
   gtk_table_attach_defaults (GTK_TABLE(table_1),button_D,3,4,3,4);

   //packing table
   gtk_box_pack_start (GTK_BOX (vbox2), table_1, TRUE, TRUE, 0);


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

   gdk_threads_leave(); 
   SET_CANCEL;

}

void KeyPad::reset() {
   Probe::reset();
}


void KeyPad::display() {


   NO_CANCEL
   gdk_threads_enter();

   //nothing to do (I think)
 

   gdk_threads_leave();
   SET_CANCEL

}

ObjectRef KeyPad::getOutput(int output_id, int count) {

 

  current_count = count;

  if (count % skip == 0) {
    char tmp[16];
    sprintf (tmp,"%d",count);
    NO_CANCEL;
    gdk_threads_enter();
    gtk_entry_set_text(GTK_ENTRY(entry1),tmp);
    gdk_threads_leave(); 
    SET_CANCEL; 
  }

  if (output_id==outputID) {
    NodeInput input = inputs[inputID];
    inputValue = input.node->getOutput(input.outputID,count);
    return inputValue;
  }
  else {
 
    if (output_id == keypadID) {
      
      if (displayEnable && (count % skip == 0))
	display();
      if (traceEnable && (count % skip == 0) && count >= breakAt)
	trace();
      
      if (changed) {
	Vector<int> *my_output = new Vector<int>(2);
	
	(*my_output)[0] = selected_line;
	(*my_output)[1] = selected_column;
	
	return ObjectRef(my_output);
	
      }
      else {
	return Object::nilObject;
    }
      
    }
    else {
      throw new NodeException (this, "KeyPad: Unknown output id", __FILE__, __LINE__);
    }

  }//else
  
 
}

void KeyPad::update_values(int pad_number) {

  //updating pad, line and column
  selected_pad = pad_number;

  switch(pad_number) {
  case pad_0_number:
    selected_line = 3;
    selected_column = 1;
    break;
  case pad_1_number:
    selected_line = 0;
    selected_column = 0;
    break;
  case pad_2_number:
    selected_line = 0;
    selected_column = 1;
    break;
  case pad_3_number:
    selected_line = 0;
    selected_column = 2;
    break;
  case pad_4_number:
    selected_line = 1;
    selected_column = 0;
    break;
  case pad_5_number:
    selected_line = 1;
    selected_column = 1;
    break;
  case pad_6_number:
    selected_line = 1;
    selected_column = 2;
    break;
  case pad_7_number:
    selected_line = 2;
    selected_column = 0;
    break;
  case pad_8_number:
    selected_line = 2;
    selected_column = 1;
    break;
  case pad_9_number:
    selected_line = 2;
    selected_column = 2;
    break;
  case pad_star_number:
    selected_line = 3;
    selected_column = 0;
    break;
  case pad_hash_number:
    selected_line = 3;
    selected_column = 2;
    break;
  case pad_A_number:
    selected_line = 0;
    selected_column = 3;
    break;
  case pad_B_number:
    selected_line = 1;
    selected_column = 3;
    break;
  case pad_C_number:
    selected_line = 2;
    selected_column = 3;
    break;
  case pad_D_number:
    selected_line = 3;
    selected_column = 3;
    break;
    
  default:
    throw new NodeException (this, "KeyPad: Invalid pad number", __FILE__, __LINE__);
    break;

  }

  changed = true;


}

void keypad_button_pressed(GtkButton  *button, KeyPad *keypad) {

  try {
  
    int pad_number = keypad->key_map[GTK_WIDGET(button)];
    keypad->update_values(pad_number);
    
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

  keypad->changed = false;

}




