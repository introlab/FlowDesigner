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

#include "mean_set.h"
#include "ObjectParser.h"

DECLARE_TYPE(MeanSet)

int MeanSet::getIDFor(Ptr<Mean> cov)
{
   for (int i=0;i<nb_means;i++)
   {
      if (cov.get()==means[i].get())
         return i;
   }
   nb_means++;
   means.resize(nb_means);
   means[nb_means-1]=cov;
   return nb_means-1;
}

Ptr<Mean> MeanSet::getPtrFor(int id) const
{
   if (id>=nb_means)
      throw GeneralException("Invalid mean ID", __FILE__, __LINE__);
   return means[id];
}


void MeanSet::printOn(ostream &out=cout) const
{
   out << "<MeanSet " << endl;
   out << "<means " << means << ">" << endl;
   out << "<nb_means " << nb_means << ">" << endl;
   out << ">\n";
}

void MeanSet::readFrom (istream &in=cin)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw ParsingException ("Parse error: '<' expected");
      in >> tag;
      if (tag == "means")
         in >> means;
      else if (tag == "nb_means")
         in >> nb_means;
      else
         throw ParsingException ("unknown argument: " + tag);

      if (!in) throw ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, MeanSet &cov)
{
   if (!isValidType(in, "MeanSet")) return in;
   cov.readFrom(in);
   return in;
}
