// Copyright (C) 1998-1999 Jean-Marc Valin & Dominic Letourneau
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

#include "Concatenate.h"
#include "FrameBinaryOperation.h"
#include "Matrix.h"
#include <typeinfo>
#include "net_types.h"
#include <string>
#include "Network.h"

Concatenate::Concatenate(string nodeName, ParameterSet params)
   : FrameBinaryOperation(nodeName, params)
{

}

void Concatenate::specificInitialize()
{
   cerr << "Concatenate initialize...\n";
   this->FrameBinaryOperation::specificInitialize();
}

void Concatenate::reset()
{
   this->FrameBinaryOperation::reset();
}

Concatenate::~Concatenate()
{
   cerr << "Destroying Concatenate\n";
}

void Concatenate::computeFrame(ObjectRef input1, ObjectRef input2, int count)
{
   //cerr << "In Concatenate::computeFrame...\n";
   const Matrix<float> &in1 = object_cast<Matrix<float> > (input1);
   const Matrix<float> &in2 = object_cast<Matrix<float> > (input2);
   Matrix<float> &out = object_cast<Matrix<float> > (output);

   int i;
   const float *input1Frame = in1[count];
   const float *input2Frame = in2[count];
   float *outputFrame = out[count];
   if (input1Length != in1.ncols())
      throw NodeException (this, "input1Length != in1.ncols()" , __FILE__, __LINE__);
   if (input2Length != in2.ncols())
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
