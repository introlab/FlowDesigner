// Copyright (C) 1999 Jean-Marc Valin

#include "CodebookMap.h"

using namespace std;

namespace FD {

DECLARE_TYPE(CodebookMap)
//@implements CMap


CodebookMap::CodebookMap(const RCPtr<VQ> &_mapIn, const vector<float *> dataIn, const vector<float *> dataOut, int length)
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
   return &mapOut[mapIn->getClassID(in)][0];
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
       throw new ParsingException ("CodebookMap::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "mapIn")
      {
         in >> mapIn;
      } else if (tag == "mapOut")
      {
         in >> mapOut;
      }

      else
         throw new ParsingException ("CodebookMap::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("CodebookMap::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("CodebookMap::readFrom : Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, CodebookMap &net)
{
   if (!isValidType(in, "CodebookMap")) return in;
   net.readFrom(in);
   return in;
}
}//namespace FD
