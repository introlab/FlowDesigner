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

#include "mean.h"

DECLARE_TYPE(Mean)


void Mean::printOn (ostream &out) const
{
   out << "<Mean " << endl;
   out << "<dimension " << dimension << "> ";
   out << "<mode " << mode << "> ";
   if (mode == accum)
      out << "<accum_count " << accum_count << "> ";
   out << endl;
   out << "<data";
   for (int i=0;i<dimension;i++)
      out << " " << operator[] (i);
   out << "> >\n";
}

void Mean::readFrom (istream &in)
{
   dimension=-1;
   string tag;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      in >> tag;
      if (tag == "dimension")
      { 
         in >> dimension;
         resize(dimension);
      }      
      else if (tag == "mode")
         in >> mode;
      else if (tag == "accum_count")
         in >> accum_count;
      else if (tag == "data")
      {
         if (dimension==-1)
            throw new ParsingException("dimension must be specified before data");
         for (int i=0;i<dimension;i++)
            in >> operator[] (i);
      } else 
         throw new ParsingException ("unknown argument: " + tag);
      if (!in) throw new ParsingException ("Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, Mean &gauss)
{
   if (!isValidType(in, "Mean")) return in;
   
   gauss.readFrom(in);

   return in;
}

