// Copyright (C) 1999 Jean-Marc Valin

#include "TextProbe.h"
#include "net_types.h"
#include "Object.h"
#include <gnome.h>
#include <sstream>

using namespace std;

namespace FD {

DECLARE_NODE(TextProbe)
/*Node
 *
 * @name TextProbe
 * @category Probe
 * @description Prints the data as text
 *
 * @input_name INPUT
 * @input_type any
 * @input_description Any data
 *
 * @output_name OUTPUT
 * @output_type any
 * @output_description Pass through
 *
 * @parameter_name BREAK_AT
 * @parameter_type int
 * @parameter_description If set, the probe runs until (count = BREAK_AT)
 *
 * @parameter_name SHOW
 * @parameter_type bool
 * @parameter_value true
 * @parameter_description Whether or not to show the the data by default
 *
 * @parameter_name SKIP
 * @parameter_type int
 * @parameter_description Count increment for each "Next"
 *
 * @parameter_name PROBE_NAME
 * @parameter_type string
 * @parameter_description Name (title) of the probe
 *
END*/


TextProbe::TextProbe(string nodeName, ParameterSet params) 
   : Probe(nodeName, params)
{
}

TextProbe::~TextProbe()
{
}

void TextProbe::initialize()
{
   Probe::initialize();

   gdk_threads_enter(); 
   GtkWidget *scrolledwindow1 = gtk_scrolled_window_new (NULL, NULL);
   gtk_widget_show (scrolledwindow1);
   gtk_container_add (GTK_CONTAINER (vbox2), scrolledwindow1);
   gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow1), GTK_POLICY_NEVER, GTK_POLICY_ALWAYS)
   ;

   
   less1 = gtk_text_view_new ();
   gtk_widget_show (less1);
   gtk_container_add (GTK_CONTAINER (scrolledwindow1), less1);
   gtk_text_view_set_editable (GTK_TEXT_VIEW (less1), FALSE);
   gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (less1), GTK_WRAP_WORD);
   gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (less1), FALSE);	 
   gdk_threads_leave(); 
}

void TextProbe::reset()
{
   Probe::reset();
}


void TextProbe::display()
{
   ostringstream out;
   
   out << *inputValue;

   gdk_threads_enter();

   if (less1) 
  {
     gtk_text_buffer_set_text (gtk_text_view_get_buffer (GTK_TEXT_VIEW (less1)),
                               out.str().c_str(), -1);
  }

   gdk_threads_leave();
}

}//namespace FD
