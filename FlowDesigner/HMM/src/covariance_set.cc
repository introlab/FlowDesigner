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

#include "covariance_set.h"
#include "ObjectParser.h"

DECLARE_TYPE(CovarianceSet)

   /*void CovarianceSet::toInvert (MeanSet &means)
{
   int i;
   means.toPtrs();
   vector <vector<float> > sums(nb_covariances);
   for (i=0;i<nb_covariances;i++)
      sums[i].resize(covariance[i].size(),0.0);
   for (i=0;i<means.size();i++)
   {
      Vector<float> &mean = means[i];
      for (int j=0;j<mean.size();j++)
         sums[i][j]+=mean[j];
   }
   for (i=0;i<nb_covariances;i++)
   {
      //covariances[i].toInvert
   }
   }*/


int CovarianceSet::getIDFor(RCPtr<Covariance> cov)
{
   for (int i=0;i<nb_covariances;i++)
   {
      if (cov.get()==covariances[i].get())
         return i;
   }
   nb_covariances++;
   covariances.resize(nb_covariances);
   covariances[nb_covariances-1]=cov;
   return nb_covariances-1;
}

RCPtr<Covariance> CovarianceSet::getPtrFor(int id) const
{
   if (id>=nb_covariances)
      throw new GeneralException("Invalid covariance ID", __FILE__, __LINE__);
   return covariances[id];
}


void CovarianceSet::printOn(ostream &out) const
{
   out << "<CovarianceSet " << endl;
   out << "<covariances " << covariances << ">" << endl;
   out << "<nb_covariances " << nb_covariances << ">" << endl;
   out << ">\n";
}

void CovarianceSet::readFrom (istream &in)
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
      if (tag == "covariances")
         in >> covariances;
      else if (tag == "nb_covariances")
         in >> nb_covariances;
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, CovarianceSet &cov)
{
   if (!isValidType(in, "CovarianceSet")) return in;
   cov.readFrom(in);
   return in;
}
