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


#include "covariance.h"
#include "gaussian.h"
#include <assert.h>
#include <iostream.h>
#include <fstream.h>
#include <iostream.h>

DECLARE_TYPE(Gaussian)

void Gaussian::to_real()
{
   covariance->processMean(mean);
   covariance->invert();
   mean->toReal();
}


Gaussian::~Gaussian()
{
}

void Gaussian::toIDsUsing (MeanSet &means, CovarianceSet & covariances)
{
   if (!using_covarianceID)
   {
   using_covarianceID=true;
   covarianceID=covariances.getIDFor(covariance);
   }
   if (!using_meanID)
   {
   using_meanID=true;
   meanID=means.getIDFor(mean);
   }
}

void Gaussian::toPtrsUsing (const MeanSet &means, const CovarianceSet & covariances)
{
   if (using_covarianceID)
   {
      using_covarianceID=false;
      covariance=covariances.getPtrFor(covarianceID);
   }
   if (using_meanID)
   {
      using_meanID=false;
      mean=means.getPtrFor(meanID);
   }
}


void Gaussian::printOn (ostream &out) const
{
   out << "<Gaussian " << endl;
   out << "<dimension " << dimension << "> ";
   out << "<accum_count " << accum_count << "> " << endl;
   if (using_meanID)
      out << "<meanID " << meanID << ">" << endl;
   else 
      out << "<mean " << mean << ">" << endl;
      
   if (using_covarianceID)
      out << "<covarianceID " << covarianceID << ">" << endl;
   else
      out << "<covariance " << covariance << ">" << endl;
   out << ">\n";
}

void Gaussian::readFrom (istream &in)
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
      else if (tag == "accum_count")
         in >> accum_count;
      else if (tag == "mean")
      {
         Ptr<Mean> tmp(new Mean);
         //Ptr<Vector<float> > tmp(new Vector<float>);
         in >> *tmp;
         mean = tmp;
         using_meanID=false;
      }
      else if (tag == "covariance")
      {
         ObjectRef tmp;
         in >> tmp;
         covariance = tmp;
         using_covarianceID=false;
      } else if (tag == "covarianceID")
      {
         in >> covarianceID;
         using_covarianceID=true;
      } else if (tag == "meanID")
      {
         in >> meanID;
         using_meanID=true;
      } else 
         throw ParsingException ("unknown argument: " + tag);
      if (!in) throw ParsingException ("Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") throw ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, Gaussian &gauss)
{
   if (!isValidType(in, "Gaussian")) return in;
   
   gauss.readFrom(in);

   return in;
}

