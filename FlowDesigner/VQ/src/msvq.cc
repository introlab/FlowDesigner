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

#include "msvq.h"

#include "msvq.h"
#include "ObjectParser.h"

DECLARE_TYPE(MSVQ)
   
MSVQ::MSVQ(const vector<int> &_stagesSizes, float (*_dist)(const float *, const float*, int) = KMeans::euclidian)
   : stagesSizes(_stagesSizes)
   , VQ(_dist)
   , stages(stagesSizes.size())
{
}

int MSVQ::ID2Vec(const vector<int> &vec) const
{
   int id=0;
   for (int i=0;i<stagesSizes.size();i++)
      id = id*stagesSizes[i] + vec[i];
   return id;
}

vector<int> MSVQ::Vec2ID(int ID) const
{
   vector<int> vec(stagesSizes.size());
   
   int curr = ID;
   int next;
   for (int i=stagesSizes.size()-1;i>=0;i--)
   {
      int next = curr/stagesSizes[i];
      vec[i] = curr - next*stagesSizes[i];
      curr = next;
   }
   
   return vec;
}


int MSVQ::nbClasses() const
{
   int ret = 1;
   for (int i=0;i<stagesSizes.size();i++)
      ret *= stagesSizes[i];
   return ret;
}

/*const vector<float> &MSVQ::operator[] (int i) const
{
   vector<float> ret(0);
   return ret;
   }*/

void MSVQ::train (const vector<float *> &data, int len, bool binary=false)
{
   length = len;
   vector<float *> train(data.size());
   float *training_data = new float [len*data.size()];
   for (int i=0;i<data.size();i++)
      train[i]=training_data+len*i;

   for (int i=0;i<data.size();i++)
      for (int j=0;j<len;j++)
	 train[i][j] = data[i][j];

   for (int i=0;i<stagesSizes.size();i++)
   {
      stages[i].train(stagesSizes[i], train, length, binary);
      
      for (int j=0;j<data.size();j++)
      {
	 const vector<float> &mean = stages[i][stages[i].getClassID(train[j])];
	 for (int k=0;k<len;k++)
	    train[j][k] -= mean[k];
      }

   }

   delete [] training_data;
}

int MSVQ::getClassID (const float *v, float *dist_return = NULL) const
{
   vector<float> remaining(length);
   for (int i=0;i<length;i++)
      remaining[i] = v[i];

   int globalID = 0;
   for (int i=0;i<stagesSizes.size();i++)
   {
      int id = stages[i].getClassID(remaining.begin(),dist_return);
      globalID = globalID*stagesSizes[i] + id;

      const vector<float> &mean = stages[i][id];
      for (int k=0;k<length;k++)
	 remaining[k] -= mean[k];  
   }
   
   return globalID;
}

/*void MSVQ::calcDist (const float *v, float *dist_return) const
{
}*/


void MSVQ::printOn(ostream &out) const
{
   out << "<MSVQ " << endl;
   out << "<length " << length << ">" << endl;
   out << "<stagesSizes " << stagesSizes << ">" << endl;
   out << "<stages " << stages << ">" << endl;
   out << ">\n";
}

void MSVQ::readFrom (istream &in)
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
      else if (tag == "stagesSizes")
         in >> stagesSizes;
      else if (tag == "stages")
         in >> stages;
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, MSVQ &mdl)
{
   if (!isValidType(in, "MSVQ")) return in;
   mdl.readFrom(in);
   return in;
}
