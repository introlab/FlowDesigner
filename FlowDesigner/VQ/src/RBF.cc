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


#include "RBF.h"
#include "ObjectParser.h"
#include "misc.h"

DECLARE_TYPE(RBF)

void RBF::train (int codeSize, const vector<float *> &data, const vector<float *> &data_out, int len, bool binary)
{
   KMeans::train (codeSize, data, len, binary);

   covar.resize(nbClasses());

   vector<int> counts(covar.size());
   for (int i=0;i<covar.size();i++)
      covar[i].resize(len,0);
   for (int i=0;i<data.size();i++)
   {
      int id = getClassID(data[i]);
      counts[id]++;
      for (int j=0;j<len;j++)
	 covar[id][j] += sqr(data[i][j] - means[id][j]);
   }

   for (int i=0;i<covar.size();i++)
   {
      float scale = 1.0/counts[i];
      for (int j=0;j<len;j++)
	 covar[i][j] = sqrt(covar[i][j]*scale);
   }

   

}

void RBF::printOn(ostream &out) const
{
   out << "<RBF " << endl;
   out << "<means " << means << ">" << endl;
   out << "<covar " << covar << ">" << endl;
   out << "<length " << length << ">" << endl;
   out << ">\n";
}

void RBF::readFrom (istream &in)
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
      if (tag == "length")
         in >> length;
      else if (tag == "covar")
         in >> covar;
      else if (tag == "means")
         in >> means;
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, RBF &mdl)
{
   if (!isValidType(in, "RBF")) return in;
   mdl.readFrom(in);
   return in;
}
