// Copyright (C) 1999 Jean-Marc Valin
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

#include "TextProbe.h"
#include "net_types.h"
#include "Object.h"
#include <gnome.h>
#include <sstream>

DECLARE_NODE(TextProbe)
/*Node
 *
 * @name TextProbe
 * @category Probe
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
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
END*/



TextProbe::TextProbe(string nodeName, ParameterSet params) 
   : Probe(nodeName, params)
{
}

TextProbe::~TextProbe()
{
}

void TextProbe::specificInitialize()
{
   Probe::specificInitialize();

   NO_CANCEL
   gdk_threads_enter(); 
   less1 = gnome_less_new ();
   gtk_widget_ref (less1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "less1", less1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (less1);
   gtk_box_pack_start (GTK_BOX (vbox2), less1, TRUE, TRUE, 0);
   gdk_threads_leave(); 
   SET_CANCEL
   //gtk_box_pack_start (GTK_BOX (vbox2), scrolledwindow2, TRUE, TRUE, 0);
}

void TextProbe::reset()
{
   Probe::reset();
}


void TextProbe::display()
{
   ostringstream out;
   
   out << *inputValue;

   NO_CANCEL
   gdk_threads_enter();

   gnome_less_clear (GNOME_LESS(less1));

   gnome_less_show_string(GNOME_LESS(less1), out.str().c_str());

   gdk_threads_leave();
   SET_CANCEL

}

