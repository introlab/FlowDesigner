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

KeyPad::KeyPad(string nodeName, ParameterSet params) 
  : BufferedNode(nodeName,params) {

  //adding outputs

  //adding parameters

}

KeyPad::~KeyPad() {
  
  gdk_threads_enter(); 
  

  //delete main widget (window)

  
  gdk_threads_leave(); 
}

void KeyPad::initialize() {
 
   //calling BufferedNode initialize()
   BufferedNode::initialize();


   gdk_threads_enter(); 


   //GTK widget creation


   gdk_threads_leave(); 
}

void KeyPad::reset() {

  //reset stuff if needed

}



void KeyPad::calculate(int output_id, int count, Buffer &out) {



}
