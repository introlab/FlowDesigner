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

#include "gaussian_set.h"
#include "ObjectParser.h"

DECLARE_TYPE(GaussianSet)

int GaussianSet::getIDFor(Ptr<Gaussian> cov)
{
   for (int i=0;i<nb_gaussians;i++)
   {
      if (cov.get()==gaussians[i].get())
         return i;
   }
   nb_gaussians++;
   gaussians.resize(nb_gaussians);
   gaussians[nb_gaussians-1]=cov;
   return nb_gaussians-1;
}

Ptr<Gaussian> GaussianSet::getPtrFor(int id) const
{
   if (id>=nb_gaussians)
      throw new GeneralException("Invalid gaussian ID", __FILE__, __LINE__);
   return gaussians[id];
}

void GaussianSet::toIDs(MeanSet & means, CovarianceSet & covariances)
{
   for (int i=0;i<nb_gaussians;i++)
   {
      gaussians[i]->toIDsUsing(means, covariances);
   }
}

void GaussianSet::toPtrs(const MeanSet & means, const CovarianceSet & covariances) const
{
   for (int i=0;i<nb_gaussians;i++)
   {
      gaussians[i]->toPtrsUsing(means, covariances);
   }
}

void GaussianSet::printOn(ostream &out=cout) const
{
   out << "<GaussianSet " << endl;
   out << "<gaussians " << gaussians << ">" << endl;
   out << "<nb_gaussians " << nb_gaussians << ">" << endl;
   out << ">\n";
}

void GaussianSet::readFrom (istream &in=cin)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("Parse error: '<' expected");
      in >> tag;
      if (tag == "gaussians")
         in >> gaussians;
      else if (tag == "nb_gaussians")
         in >> nb_gaussians;
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, GaussianSet &cov)
{
   if (!isValidType(in, "GaussianSet")) return in;
   cov.readFrom(in);
   return in;
}
