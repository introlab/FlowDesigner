// Copyright (C) 1998-1999  Jean-Marc Valin
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
#ifndef MEAN_H
#define MEAN_H

#include <math.h>
#include <vector>
#include "Object.h"
#include "ObjectParser.h"
#include "misc.h"
#include "Vector.h"

class Mean : public Vector<float>
{
protected:
   int mode;
   int accum_count;
public:
   Mean() 
      : Vector<float>() 
      , mode(0)
      , accum_count(0)
   {}

   Mean(int n, const float &x = 0) 
      : Vector<float>(n, x)
      , mode(0)
      , accum_count(0)
   {}

};

#endif
