// Copyright (C) 1999 Jean-Marc Valin
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

#include "acoustic_model.h"
#include "ObjectParser.h"
#include "Object.h"

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

void AcousticModel::printOn(ostream &out=cout) const
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

void AcousticModel::readFrom (istream &in=cin)
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
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, AcousticModel &mdl)
{
   if (!isValidType(in, "AcousticModel")) return in;
   mdl.readFrom(in);
   return in;
}
