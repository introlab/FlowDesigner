// Copyright (C) 1999 Jean-Marc Valin

#include "acoustic_model.h"
#include "ObjectParser.h"
#include "Object.h"

using namespace std;

namespace FD {

DECLARE_TYPE(AcousticModel)

void AcousticModel::toIDs()
{
   gmms.toIDs(gaussians);
   gaussians.toIDs(means, covariances);
}

void AcousticModel::toPtrs()
{
   gaussians.toPtrs(means, covariances);
   gmms.toPtrs(gaussians);
   
}

void AcousticModel::printOn(ostream &out) const
{
   out << "<AcousticModel " << endl;
   out << "<covariances " << covariances << ">" << endl;
   out << "<means " << means << ">" << endl;
   out << "<gaussians " << gaussians << ">" << endl;
   out << "<gmms " << gmms << ">" << endl;
   //out << "<states " << states << ">" << endl;
   //out << "<phones " << phones << ">" << endl;
   out << ">\n";
}

void AcousticModel::readFrom (istream &in)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("AcousticModel::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "covariances")
         in >> covariances;
      else if (tag == "means")
         in >> means;
      else if (tag == "gaussians")
         in >> gaussians;
      else if (tag == "gmms")
         in >> gmms;
      //else if (tag == "states")
      //   in >> states;
      //else if (tag == "phones")
      //   in >> phones;
      else
         throw new ParsingException ("AcousticModel::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("AcousticModel::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("AcousticModel::readFrom : Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, AcousticModel &mdl)
{
   if (!isValidType(in, "AcousticModel")) return in;
   mdl.readFrom(in);
   return in;
}

}//namespace FD
