// Copyright (C) 1998-1999  Jean-Marc Valin


#include "covariance.h"
#include "gaussian.h"
#include <assert.h>
#include <fstream>
#include <iostream>

DECLARE_TYPE(Gaussian)
//@implements GMM

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
         RCPtr<Mean> tmp(new Mean);
         //RCPtr<Vector<float> > tmp(new Vector<float>);
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
         throw new ParsingException ("Gaussian::readFrom : unknown argument: " + tag);
      if (!in) throw new ParsingException ("Gaussian::readFrom : Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") throw new ParsingException ("Gaussian::readFrom : Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, Gaussian &gauss)
{
   if (!isValidType(in, "Gaussian")) return in;
   
   gauss.readFrom(in);

   return in;
}

