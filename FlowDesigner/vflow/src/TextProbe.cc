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
#include <strstream>

//DECLARE_NODE(Probe)
NODE_INFO(TextProbe, "Probe", "INPUT", "OUTPUT", "")


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

   gdk_threads_enter(); 
   less1 = gnome_less_new ();
   gtk_widget_ref (less1);
   gtk_object_set_data_full (GTK_OBJECT (window1), "less1", less1,
			     (GtkDestroyNotify) gtk_widget_unref);
   gtk_widget_show (less1);
   gtk_box_pack_start (GTK_BOX (vbox2), less1, TRUE, TRUE, 0);
   gdk_threads_leave(); 
   
   //gtk_box_pack_start (GTK_BOX (vbox2), scrolledwindow2, TRUE, TRUE, 0);
}

void TextProbe::reset()
{
   Probe::reset();
}


void TextProbe::trace()
{
   char probeOut[1000];
   ostrstream out(probeOut, 999);
   
   out << *inputValue;
   cerr << "Probe value = " << *inputValue << endl;
   gdk_threads_enter();
   gnome_less_clear (GNOME_LESS(less1));
   gnome_less_show_string(GNOME_LESS(less1), probeOut);
   gdk_threads_leave(); 
   
   Probe::trace();
}

void TextProbe::next()
{
   Probe::next();
}
