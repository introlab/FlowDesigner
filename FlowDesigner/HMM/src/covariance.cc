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

void DiagonalCovariance::processMean(Ptr<Mean> mean)
{
   if (mode == inverted) return;
   if (mode != accum) throw string("DiagonalCovariance::processMean");

   Mean &v=*mean;
   float accum_1 = 1.0/v.getAccum();
   for(int i = 0; i < dimension; i++ )
      data[i] -= sqr(v[i])*accum_1;
}

void DiagonalCovariance::invert()
{
   if (mode == inverted) return;
   if (mode != accum) throw string("DiagonalCovariance::invert");
   float accum_1 = 1.0/accum_count;
for(int i = 0; i < data.size(); i++ )
   {
      data[i] = 1.0 / (.001 + data[i] * accum_1);
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
   accum_count=0;
}

void DiagonalCovariance::printOn(ostream &out) const
{
   out << "<DiagonalCovariance" << endl;
   out << "<dimension " << dimension << ">" << endl;
   out << "<mode " << mode << ">" << endl;
   if (mode == accum)
      out << "<accum_count " << accum_count << "> " << endl;
   out << "<data";
   for (int i=0;i<dimension;i++)
      out << " " << data[i];

   out << ">\n";
   return;
}

void DiagonalCovariance::readFrom (istream &in)
{
   dimension=-1;
   string tag;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      in >> tag;
      if (tag == "dimension")
      {
         in >> dimension;
         data.resize(dimension);
      } else if (tag == "mode")
         in >> mode;
            else if (tag == "accum_count")
         in >> accum_count;
      else if (tag == "data")
      {
         if (dimension==-1)
            throw ParsingException("dimension must be specified before data");
         for (int i=0;i<dimension;i++)
            in >> data[i];
      } else 
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
