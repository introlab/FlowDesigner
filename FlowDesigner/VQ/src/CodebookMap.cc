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

#include "CodebookMap.h"

DECLARE_TYPE(CodebookMap)


CodebookMap::CodebookMap(const Ptr<VQ> &_mapIn, const vector<float *> dataIn, const vector<float *> dataOut, int length)
   : mapIn(_mapIn)
   , mapOut(_mapIn->nbClasses(),Vector<float> (length,0))
{
   //for (int i=0;i<_mapIn.nbClasses();i++)
   //   mapOut[i].resize(length);
   
   int mapSize = mapIn->nbClasses();
   vector<int> counts(mapSize,0);
   
   for (int i=0;i<dataIn.size();i++)
   {
      int id = mapIn->getClassID(dataIn[i]);
      for (int j=0;j<length;j++)
	 mapOut[id][j] += dataOut[i][j];
      counts[id]++;
   }
   
   for (int i=0;i<mapSize;i++)
   {
      for (int j=0;j<length;j++)
	 mapOut[i][j] /= counts[i];
   }

   double dist=0;
   for (int i=0;i<dataIn.size();i++)
   {
      int id = mapIn->getClassID(dataIn[i]);
      for (int j=0;j<length;j++)
	 dist += (mapOut[id][j] - dataOut[i][j]) * (mapOut[id][j] - dataOut[i][j]);
   }
   cerr << "length is: " << length << endl;
   cerr << "codebook map dist = " << dist/(length*dataIn.size()) << endl;
}

const float * CodebookMap::calcOutput(const float *in) const
{
   //looks up the input vector, uses the ID to find the corresponding output
   return mapOut[mapIn->getClassID(in)].begin();
}

void CodebookMap::printOn(ostream &out) const
{
   out << "<CodebookMap " << endl;
   out << "<mapIn " << mapIn << ">" << endl;
   out << "<mapOut " << mapOut << ">" << endl;
   out << ">\n";
}


void CodebookMap::readFrom (istream &in)
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
      if (tag == "mapIn")
      {
         in >> mapIn;
      } else if (tag == "mapOut")
      {
         in >> mapOut;
      }

      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, CodebookMap &net)
{
   if (!isValidType(in, "CodebookMap")) return in;
   net.readFrom(in);
   return in;
}
