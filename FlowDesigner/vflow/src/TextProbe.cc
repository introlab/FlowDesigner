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
}

void TextProbe::reset()
{
   Probe::reset();
}


void TextProbe::trace()
{
   cerr << "Probe value = " << *inputValue << endl;
   Probe::trace();
}

void TextProbe::next()
{
   Probe::next();
}
