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

#include "audioinfo.h"
#include "ObjectParser.h"

void AudioInfo::printOn(ostream &out=cout) const
{
   out << "<AudioInfo " << endl;
   out << "<ortho " << ortho << ">" << endl;
   if (coarse_endpointed)
   {
      out << "<coarse_start " << coarse_start << ">" << endl;
      out << "<coarse_end " << coarse_end << ">" << endl;
   }
   if (fine_endpointed)
   {
      out << "<fine_start " << fine_start << ">" << endl;
      out << "<fine_start " << fine_start << ">" << endl;
   }
   out << ">\n";
}

void AudioInfo::readFrom (istream &in)
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
      if (tag == "ortho")
         in >> ortho;
      else if (tag == "coarse_start")
      {
         in >> coarse_start;
         coarse_endpointed=true;
      } else if (tag == "coarse_end")
      {
         in >> coarse_end;
         coarse_endpointed=true;
      } else if (tag == "fine_start")
      {
         in >> fine_start;
         fine_endpointed=true;
      } else if (tag == "coarse_end")
      {
         in >> fine_end;
         fine_endpointed=true;
      } else
         throw ParsingException ("unknown argument: " + tag);
      
      if (!in) throw ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw ParsingException ("Parse error: '>' expected ");
   }

}

istream &operator >> (istream &in, AudioInfo &info)
{
   if (!isValidType(in, "AudioInfo")) return in;
   info.readFrom(in);
   return in;
}
