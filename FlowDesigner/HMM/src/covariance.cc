// Copyright (C) 1998-1999 Jean-Marc Valin & Daniel Kiecza
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
#include "covariance.h"
#include "math.h"

void DiagonalCovariance::to_real(const float accum_1, const vector<float> *mean)
{
#ifdef DEBUG
   cerr << "accum_1 #2: " << accum_1 << endl;
   cerr << "*tata* "<<endl;
#endif
   for(unsigned int i = 0; i < data.size(); i++ )
   {
      data[i] = .01 + data[i] * accum_1 - sqr( (*mean)[i] ) ;
   }
}


void DiagonalCovariance::compute_determinant() const
{
   determinant=1;
   for (unsigned int i=0;i<dimension;i++)
      determinant *= data[i];
   //determinant = .5*log(determinant);
   determinant_is_valid = true;
}

void DiagonalCovariance::reset()
{
   determinant_is_valid=false;
   for (unsigned int i=0;i<dimension;i++)
      data[i]=0.0;
}
