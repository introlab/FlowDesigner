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

#define _PLUGIN_NODE

#include "Concatenate.h"
#include "FrameBinaryOperation.h"
#include "Buffer.h"
#include <typeinfo>
#include "net_types.h"
#include <string>
#include "Network.h"

DECLARE_NODE(Concatenate)

Concatenate::Concatenate(string nodeName, ParameterSet params)
   : FrameBinaryOperation(nodeName, params)
{

}

void Concatenate::specificInitialize()
{
   //cerr << "Concatenate initialize...\n";
   this->FrameBinaryOperation::specificInitialize();
}

void Concatenate::reset()
{
   this->FrameBinaryOperation::reset();
}

Concatenate::~Concatenate()
{
   //cerr << "Destroying Concatenate\n";
}

void Concatenate::computeFrame(ObjectRef input1, ObjectRef input2, int count)
{
   //cerr << "In Concatenate::computeFrame...\n";
   const Buffer &in1 = object_cast<Buffer> (input1);
   const Buffer &in2 = object_cast<Buffer> (input2);
   Buffer &out = object_cast<Buffer> (output);

   int i;
   const Vector<float> &input1Frame = object_cast<Vector<float> > (in1[count]);
   const Vector<float> &input2Frame = object_cast<Vector<float> > (in2[count]);
   Vector<float> &outputFrame = object_cast<Vector<float> > (out[count]);
   if (input1Length != input1Frame.size())
      throw NodeException (this, "input1Length != in1.ncols()" , __FILE__, __LINE__);
   if (input2Length != input2Frame.size())
      throw NodeException (this, "input2Length != in2.ncols()" , __FILE__, __LINE__);
   if (input1Length+input2Length != outputLength)
      throw NodeException (this, "input1Length+input2Length != outputLength" , __FILE__, __LINE__);
   for (i=0;i<input1Length;i++)
   {
      outputFrame[i]= input1Frame[i];
   }
   for (i=0;i<input2Length;i++)
   {
      outputFrame[i+input1Length]= input2Frame[i];
   }
}

