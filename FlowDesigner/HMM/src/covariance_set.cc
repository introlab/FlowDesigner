// Copyright (C) 1998-1999 Jean-Marc Valin

#include "covariance_set.h"
#include "ObjectParser.h"

using namespace std;

namespace FD {

DECLARE_TYPE(CovarianceSet)
//@implements GMM

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
       throw new ParsingException ("CovarianceSet::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "covariances")
         in >> covariances;
      else if (tag == "nb_covariances")
         in >> nb_covariances;
      else
         throw new ParsingException ("CovarianceSet::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("CovarianceSet::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("CovarianceSet::readFrom : Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, CovarianceSet &cov)
{
   if (!isValidType(in, "CovarianceSet")) return in;
   cov.readFrom(in);
   return in;
}

}//namespace FD
