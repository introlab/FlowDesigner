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


#include "kmeans.h"
#include "ObjectParser.h"

DECLARE_TYPE(KMeans)

int KMeans::split (const vector<float *> &data, int len)
{
   int nbMeans = means.size();
   float totalDist[nbMeans];
   int belongs[data.size()];
   int accum[data.size()];
   int i;
   
   for (i=0; i<nbMeans;i++)
      totalDist[i]=0;
   for (i=0; i<nbMeans;i++)
      accum[i]=0;
   
   for (i=0; i<data.size();i++)
   {
      float tmp;
      belongs[i] = getClassID(data[i], &tmp);
      totalDist[belongs[i]] += tmp;
   }
   
   float max_dist = 0;
   int maxID;
   for (i=0; i<nbMeans;i++)
      if (totalDist[i]/accum[i] > max_dist)
      {
         max_dist=totalDist[i]/accum[i];
         maxID=i;
      }
   
   means.resize(nbMeans+1);
   means[nbMeans].resize(length);
   for (i=0; i<length;i++)
   {
      float factor = .99 + ((rand() % 2000) *.00001);
      //factor = 1.01;
      means[nbMeans][i]=means[maxID][i]*factor;
      //cerr << means[nbMeans][i] << " " << means[maxID][i] << endl;
   }
   nbMeans++;
}

void KMeans::update (const vector<float *> &data, int len)
{
   int nbMeans = means.size();
   float totalDist[nbMeans];
   int belongs[data.size()];
   int accum[data.size()];
   int i,j;
   
   for (i=0; i<nbMeans;i++)
      totalDist[i]=0;
   for (i=0; i<nbMeans;i++)
      accum[i]=0;
   
   for (i=0; i<data.size();i++)
   {
      float tmp;
      belongs[i] = getClassID(data[i], &tmp);
      totalDist[belongs[i]] += tmp;
   }
   
   for (i=0;i<nbMeans;i++)
      for (j=0;j<length;j++)
         means[i][j]=0;
   
   for (i=0; i<data.size();i++)
   {
      int meanID=belongs[i];
      //cerr << "meanID = " << meanID << endl;
      accum[meanID]++;
      for (j=0;j<length;j++)
         means[meanID][j] += data[i][j];
   }
   for (i=0; i<nbMeans;i++)
   {
      float accum_1 = 1.0/accum[i];
      //cerr << "mean " << i << ": (accum = " << accum[i] << ") ";
      for (j=0;j<length;j++)  
      {
         //cerr << means[i][j] << " ";
         means[i][j] *= accum_1;
         //cerr << means[i][j] << " ";
      }
      //cerr << endl;
   }
   
   
}

void KMeans::train (int codeSize, const vector<float *> &data, int len)
{
   int i,j;
   length=len;
   means.resize(1);
   means[0].resize(length);
   //accum.resize(1);
   for (i=0;i<length;i++)
      means[0][i] = 0;
   for (i=0;i<data.size();i++)
      for (j=0;j<length;j++)
         means[0][j] += data[i][j];
   //accum[0]=data.size();
   for (j=0;j<length;j++)
      means[0][j] /= data.size();
   int splitID=0;
   for (i=1;i<codeSize;i++)
   {
      split (data, len);
      for (j=0;j<2;j++)
         update(data, len);
   }
}

int KMeans::getClassID (const float *v, float *dist_return = NULL) const
{
   float min_dist = dist(means[0].begin(), v, length);
   int minID=0;
   for (int i=1;i<means.size();i++)
   {
      float tmp = dist(means[i].begin(), v, length);
      if (tmp < min_dist) 
      {
         minID=i;
         min_dist=tmp;
      }
   }
   if (dist_return)
      *dist_return = min_dist;
   //cerr << "classID: " << minID << endl;
   return minID;
}

void KMeans::printOn(ostream &out=cout) const
{
   out << "<KMeans " << endl;
   out << "<means " << means << ">" << endl;
   out << "<length " << length << ">" << endl;
   out << ">\n";
}

void KMeans::readFrom (istream &in=cin)
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
      if (tag == "length")
         in >> length;
      else if (tag == "means")
         in >> means;
      else
         throw ParsingException ("unknown argument: " + tag);

      if (!in) throw ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw ParsingException ("Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, KMeans &mdl)
{
   if (!isValidType(in, "KMeans")) return in;
   mdl.readFrom(in);
   return in;
}
