// Copyright (C) 1998-1999 Jean-Marc Valin

#include "gaussian_set.h"
#include "ObjectParser.h"

using namespace std;
using namespace FD;

DECLARE_TYPE(GaussianSet)
//@implements GMM

int GaussianSet::getIDFor(RCPtr<Gaussian> cov)
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

RCPtr<Gaussian> GaussianSet::getPtrFor(int id) const
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

void GaussianSet::printOn(ostream &out) const
{
   out << "<GaussianSet " << endl;
   out << "<gaussians " << gaussians << ">" << endl;
   out << "<nb_gaussians " << nb_gaussians << ">" << endl;
   out << ">\n";
}

void GaussianSet::readFrom (istream &in)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("GaussianSet::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "gaussians")
      {
         in >> gaussians;
      } else if (tag == "nb_gaussians")
         in >> nb_gaussians;
      else
         throw new ParsingException ("GaussianSet::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("GaussianSet::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("GaussianSet::readFrom : Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, GaussianSet &cov)
{
   if (!isValidType(in, "GaussianSet")) return in;
   cov.readFrom(in);
   return in;
}
