// Copyright (C) 1998-1999 Jean-Marc Valin
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
#include "misc.h"
#include <string>

DECLARE_TYPE(DiagonalCovariance)

void DiagonalCovariance::to_invert(const float accum_1, const vector<float> *mean)
{
   if (mode == inverted) return;
   if (mode != accum) throw string("DiagonalCovariance::to_invert");
#ifdef DEBUG
   cerr << "accum_1 #2: " << accum_1 << endl;
   cerr << "*tata* "<<endl;
#endif
   for(unsigned int i = 0; i < data.size(); i++ )
   {
      data[i] = 1.0 / (.001 + data[i] * accum_1 - sqr( (*mean)[i] ) );
   }
   mode = inverted;
}

float DiagonalCovariance::mahalanobisDistance(const float *x1, const float *x2) const
{
   if (mode != inverted) throw string ("DiagonalCovariance::mahalanobisDistance");
   float dist=0;
   for (int i=0;i<dimension;i++)
      dist += sqr(x1[i]-x2[i]) * data[i];
   return dist-getDeterminant();
}

void DiagonalCovariance::compute_determinant() const
{
   if (mode != inverted) throw string ("DiagonalCovariance::compute_determinant");
   determinant=0;

   /*for (unsigned int i=0;i<dimension;i++)
      determinant *= data[i];
   determinant = .5*log(determinant);
   */

   for (unsigned int i=0;i<dimension;i++)
      determinant += .5*log(data[i]);

   determinant_is_valid = true;
}

void DiagonalCovariance::reset()
{
   determinant_is_valid=false;
   for (unsigned int i=0;i<dimension;i++)
      data[i]=0.0;
   mode = accum;
}

void DiagonalCovariance::printOn(ostream &out) const
{
   out << "<DiagonalCovariance" << endl;
   out << "<dimension " << dimension << ">" << endl;
   out << "<data " << data << ">" << endl;
   out << "<mode " << mode << ">" << endl;
   out << ">\n";
   return;
}

void DiagonalCovariance::readFrom (istream &in)
{
   string tag;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      in >> tag;
      if (tag == "dimension") 
         in >> dimension;
      else if (tag == "data")
         in >> data;
      else if (tag == "mode")
         in >> mode;
      else 
         throw ParsingException ("unknown argument: " + tag);
      if (!in) throw ParsingException ("Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") throw ParsingException ("Parse error: '>' expected ");
   }   
};

istream &operator >> (istream &in, DiagonalCovariance &cov)
{
   if (!isValidType(in, "DiagonalCovariance")) return in;

   cov.readFrom(in);   
   return in;
}
