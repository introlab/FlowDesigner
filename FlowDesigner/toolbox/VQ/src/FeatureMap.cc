// Copyright (C) 2001 Jean-Marc Valin

#include "FeatureMap.h"
#include <string>
#include "ObjectParser.h"
#include "misc.h"
#include <algorithm>

#ifdef HAVE_VALUES_H
#include <values.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif


using namespace std;

namespace FD {

DECLARE_TYPE(FeatureMap)
//@implements FeatureMap

void FeatureMap::recursiveSplit (const vector<float *> &inData, const vector<float *> &outData, int level)
{
   if (level <= 0) 
   //if (data.size() < 50) 
   {
      DYN_VEC(float, outDimension, mean);
      //float mean[outDimension];
      for (int i=0;i<outDimension;i++)
	 mean[i]=0;
         //mapData[i]=0;
      for (size_t k=0;k<outData.size();k++)
	 for (int i=0;i<outDimension;i++)
	    mean[i]+=outData[k][i];
	    //mapData[i]+=outData[k][i];

      if (outData.size() != 0)
      {
	 float sum_dist=0;
	 for (int i=0;i<outDimension;i++)
	    mean[i]/=outData.size();
	 //mapData[i]/=outData.size();
      
	 float mindist = FLT_MAX;
	 int closest = 0;
	 for (size_t k=0;k<outData.size();k++)
	 {
	    float dist=0;
	    for (int i=0;i<outDimension;i++)
	       dist += (outData[k][i]-mean[i]) * (outData[k][i]-mean[i]);
	    if (dist < mindist)
	    {
	       closest=k;
	       mindist=dist;
	    }
	    sum_dist+=dist;
	 }
	 cout << "average dist = " << sum_dist/outData.size()/outDimension << endl;
	 //cerr << "closest = " << closest << "\t dist = " << mindist << endl;
	 for (int i=0;i<outDimension;i++)
	    mapData[i] = outData[closest][i];
	 
      } else {
	 for (int i=0;i<outDimension;i++)
	 mapData[i] = 0;
      }
      
      
      cout << "LEAF: " << inData.size() << endl;
      return;
   }
   int dim;
   float thresh;
   split(inData, outData, dim, thresh);
   
   
   vector<float *> firstInData;
   vector<float *> secondInData;
   vector<float *> firstOutData;
   vector<float *> secondOutData;
   for (size_t i=0;i<inData.size();i++)
   {
      bool less=false;
      if (inData[i][dim] < thresh)
	 less=true;
      if (inData[i][dim] == thresh)
	 less=rand()&1;
      if (less)
      {
         firstInData.insert(firstInData.end(), inData[i]);
         firstOutData.insert(firstOutData.end(), outData[i]);
      } else {
	 if (inData[i][dim] == thresh)
	    cout << "equal: " << dim << " " << thresh << endl;
         secondInData.insert(secondInData.end(), inData[i]);
         secondOutData.insert(secondOutData.end(), outData[i]);
      }
   }
   splitDimension  = dim;
   threshold = thresh;
   //cout << dimension << endl;
   first = new FeatureMap (inDimension, outDimension);
   second = new FeatureMap (inDimension, outDimension);
   terminal = false;
   
   
   first->recursiveSplit(firstInData, firstOutData, level-1);
   second->recursiveSplit(secondInData, secondOutData, level-1);
}

void FeatureMap::split(const vector<float *> &inData, const vector<float *> &outData, int &bestDim, float &bestThreshold)
{
   bestDim=0;
   int nbEqual=0;
   bestThreshold=0;
   float bestMutual = -FLT_MAX;
   for (int i=0;i<inDimension;i++)
   {
      float threshold;
      float currentMutual;
      findThreshold(inData, outData, i, threshold, currentMutual);
      //cerr << "threshold: " << threshold << " currentMutual: " << currentMutual << endl;
      bool isBest = false;
      if (currentMutual > bestMutual)
      {
	 isBest=true;
	 nbEqual=0;
      }
      if (currentMutual == bestMutual)
      {
	 nbEqual++;
	 if (rand()%nbEqual==0)
	    isBest=true;
      }
      if (isBest)
      {
         bestMutual=currentMutual;
         bestDim=i;
         bestThreshold=threshold;
      }
   }
   cerr << "bestDim: " << bestDim << " bestThreshold: " << bestThreshold << endl;
   //if (some condition on bestMutual) don't perform the split
   //splitWithThreshold(data, bestDim, bestThreshold);
}




//find threshold using split at median and mutual information
void FeatureMap::findThreshold(const vector<float *> &inData, const vector<float *> &outData, int dim, float &thresh, float &score)
{


   if (inData.size()==0) thresh=0; 
   else {
      //FIXME: Allocation for sorted should be done faster, but with risk of 
      //smashing the stack
      //float sorted[inData.size()];
      float *sorted = new float [inData.size()];

      for (size_t i=0;i<inData.size();i++)
         sorted[i] = inData[i][dim];
      //qsort(sorted,data.size(),sizeof(float), float_less);
      sort (sorted,sorted+inData.size());
      thresh=sorted[inData.size()/2];
      delete [] sorted;
   }
   
   //float sumA[outDimension];
   //float sumB[outDimension];
   //float s2A[outDimension];
   //float s2B[outDimension];
   DYN_VEC(float, outDimension, sumA);
   DYN_VEC(float, outDimension, sumB);
   DYN_VEC(float, outDimension, s2A);
   DYN_VEC(float, outDimension, s2B);

   for (int i=0;i<outDimension;i++)
   {
      sumA[i]=sumB[i]=s2A[i]=s2B[i]=0;
   }

   for (size_t k=0;k<inData.size();k++)
   {
      bool greater=false;
      if (inData[k][dim] > thresh)
	 greater=true;
      if (inData[k][dim] == thresh)
	 greater=rand()&1;
      if (greater)
      {
	 for (int i=0;i<outDimension;i++)
	 {
	    sumA[i]+=outData[k][i];
	    s2A[i]+=outData[k][i]*outData[k][i];
	 }
      } else {
	 for (int i=0;i<outDimension;i++)
	 {
	    sumB[i]+=outData[k][i];
	    s2B[i]+=outData[k][i]*outData[k][i];
	 }
      }
   }
   
   score = 0;
   for (int i=0;i<outDimension;i++)
   {
      score += s2A[i] - (sumA[i]*sumA[i]/inData.size());
      score += s2B[i] - (sumB[i]*sumB[i]/inData.size());
   }

   score = -score;
}


int FeatureMap::setNumbering(int start)
{
   if (terminal)
   {
      cellID=start;
      //cerr << start << endl;
      return start+1;
   } else {
      return second->setNumbering(first->setNumbering(start));
   } 
}

void FeatureMap::calc(const float *in, float *out)
{
   if (terminal) 
   {
      //cerr << "found!\n";
      //cerr << mapData.size() << endl;
      for (int i=0;i<outDimension;i++)
	 out[i]=mapData[i];
      return;
   }
   //cerr << "+";
   if (in[splitDimension] < threshold) 
      first->calc(in, out);
   else
      second->calc(in, out);

}

/*
int FeatureMap::belongs(float *vect) const
{
   if (terminal) return cellID;
   
   if (vect[splitDimension] < threshold) 
      return first->belongs(vect);
   else
      return second->belongs(vect);
}

void FeatureMap::calcTemplate (const vector<float *> &features, vector<int> &templ) const
{
   for (vector<float *>::const_iterator feature = features.begin(); 
        feature < features.end(); feature++)
   {
      //cerr << "(" << (*feature)[0] << "," << (*feature)[1] << "): " << belongs(*feature) << endl;
      templ[belongs(*feature)]++;
   }
}
*/

void FeatureMap::printOn(ostream &out) const
{
   out << "<FeatureMap " << endl;
   out << "<inDimension " << inDimension << ">" << endl;
   out << "<outDimension " << outDimension << ">" << endl;
   //out << "<numberClasses " << numberClasses << ">" << endl;
   out << "<terminal " << terminal << ">" << endl;
   if (terminal)
   {
      out << "<cellID " << cellID << ">" << endl;
      out << "<mapData " << mapData << ">" << endl;
   } else {
      out << "<threshold " << threshold << ">" << endl;
      out << "<splitDimension " << splitDimension << ">" << endl;
      out << "<first " << *first << ">" << endl;;
      out << "<second " << *second << ">" << endl;;
   }
   
   out << ">\n";
}

ostream &operator << (ostream &out, const FeatureMap &cell)
{
   cell.printOn(out);
   return out;
}

void FeatureMap::readFrom (istream &in)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      in >> tag;
      if (tag == "inDimension") 
         in >> inDimension;
      else if (tag == "outDimension") 
         in >> outDimension;
      //else if (tag == "numberClasses")
      //   in >> numberClasses;

      else if (tag == "terminal")
         in >> terminal;

      else if (tag == "cellID")
         in >> cellID;

      else if (tag == "threshold")
         in >> threshold;

      else if (tag == "mapData")
         in >> mapData;

      else if (tag == "splitDimension")
         in >> splitDimension;

      else if (tag == "first")
      {
         FeatureMap *tmp = new FeatureMap;
         in >> *tmp;
         first = tmp;
      }
      else if (tag == "second")
      {
         FeatureMap *tmp = new FeatureMap;
         in >> *tmp;
         second = tmp;
      } else 
         throw new ParsingException ("FeatureMap::readFrom : unknown argument: " + tag);
      if (!in) throw new ParsingException ("FeatureMap::readFrom : Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") throw new ParsingException ("FeatureMap::readFrom : Parse error: '>' expected ");
   }
   
}

istream &operator >> (istream &in, FeatureMap &cell)
{
   if (!isValidType(in, "FeatureMap")) 
      return in;
   cell.readFrom(in);
   return in;
}
}//namespace FD
