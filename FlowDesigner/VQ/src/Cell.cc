// Copyright (C) 2001 Jean-Marc Valin

#include "Cell.h"
#include <string>
#include "ObjectParser.h"
#include "misc.h"
#include <algorithm>

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

DECLARE_TYPE(Cell)
//@implements VQ

void Cell::recursiveSplit (const vector<pair<int, float *> > &data, int level)
{
   if (level <= 0) 
   //if (data.size() < 50) 
   {
      cout << "LEAF: " << data.size() << endl;
      return;
   }
   int dim;
   float thresh;
   //cerr << "aa\n";
   split(data, dim, thresh);
   
   //cerr << "bb\n";
   vector<pair<int, float *> > firstData;
   vector<pair<int, float *> > secondData;
   for (int i=0;i<data.size();i++)
      if (data[i].second[dim] < thresh)
      {
         //cerr << i << "(" << data[i].second[0] << "," << data[i].second[1] << ") goes to first\n";
         firstData.insert(firstData.end(), data[i]);
      } else {
         //cerr << i << "(" << data[i].second[0] << "," << data[i].second[1] << ") goes to second\n";
         secondData.insert(secondData.end(), data[i]);
      }
   splitDimension  = dim;
   threshold = thresh;
   //cout << dimension << endl;
   first = new Cell (dimension, numberClasses);
   second = new Cell (dimension, numberClasses);
   terminal = false;
   
   
   first->recursiveSplit(firstData, level-1);
   second->recursiveSplit(secondData, level-1);
}

void Cell::split(const vector<pair<int, float *> > &data, int &bestDim, float &bestThreshold)
{
   bestDim=0;
   int nbEqual=0;
   bestThreshold=0;
   float bestMutual = -FLT_MAX;
   for (int i=0;i<dimension;i++)
   {
      //cerr << "dim " << i << endl;
      float threshold;
      float currentMutual;
      findThreshold(data, i, threshold, currentMutual);
      //cerr << "threshold: " << threshold << " currentMutual: " << currentMutual << endl;
      bool isBest = false;
      if (currentMutual > bestMutual)
      {
	 isBest=true;
	 nbEqual=0;
      }
      if (currentMutual == bestMutual)
      {
	 cerr << "randomizing at " << currentMutual << " size = " << data.size() << "\n";
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
   //cerr << "bestDim: " << bestDim << " bestThreshold: " << bestThreshold << endl;
   //if (some condition on bestMutual) don't perform the split
   //splitWithThreshold(data, bestDim, bestThreshold);
}

/*void Cell::findThreshold(const vector<pair<int, float *> > &data, int dim, float &bestThresh, float &bestScore)
{
   if (data.size()==0) 
   {
      bestThresh=0;
      bestScore = 0;
      return;
   }
   float min_value = FLT_MAX, max_value = -FLT_MAX;
   int min_ind = 0, max_ind = 0;
   int i,k;
   for (i=0;i<data.size();i++)
   {
      if (data[i].second[dim] > max_value) 
      {
         max_value = data[i].second[dim];
         max_ind=i;
      }
      if (data[i].second[dim] < min_value)
      {
         min_value = data[i].second[dim];
         min_ind=i;
      }
   }

   bestThresh = 0;
   bestScore = -FLT_MAX;
   float thresh;
   float score;
   for (thresh = min_value; thresh < max_value; thresh += (max_value-min_value)/15.0)
   {
      int sumAi = 0, sumBi = 0;
      vector<int> Ai (numberClasses, 0);
      vector<int> Bi (numberClasses, 0);
      for (k=0;k<data.size();k++)
      {
         if (data[k].second[dim] >= thresh) 
         {
            sumAi++;
            Ai[data[k].first]++;
         } else {
            sumBi++;
            Bi[data[k].first]++;
         }
      }
      
      double weight = double(sumAi)/data.size();
      //cerr << "weight: " << weight << "   sumAi = " << sumAi << endl;
      score = 0.0;
      for (i = 0;i<numberClasses;i++)
      {
         //cerr << "A[" << i << "] = " << Ai[i] << "\t" << "Ai[i] / sumAi = " << (double( Ai[i] ) / sumAi ) << "\t";
         //cerr << "B[" << i << "] = " << Bi[i] << "\t" << "Bi[i] / sumBi = " << (double( Bi[i] ) / sumBi ) << "\t";
         if (sumAi)
            score -=    weight  *  entropy_funct (double( Ai[i] ) / sumAi );
         if (sumBi)
            score -= (1-weight) *  entropy_funct (double( Bi[i] ) / sumBi );
         //cerr << "score = " << score << endl;
      }
      cerr << "got " << score << " for threshold " << thresh << endl;
      if (score > bestScore)
      {
         bestThresh = thresh;
         bestScore = score;
      }
   }
   }*/

/*
void Cell::findThreshold(const vector<pair<int, float *> > &data, int dim, float &bestThresh, float &bestScore)
{
   float sum = 0, s2 = 0;
   int i,k;
   for (i=0;i<data.size();i++)
   {
      sum += data[i].second[dim];
      s2+= sqr(data[i].second[dim]);
   }
   if (data.size()<=1)
   {
      bestThresh=0;
      bestScore=0;
      return;
   }
   sum /= data.size();
   s2=sqrt(s2/data.size() - sqr(sum) );
   //cerr << "s2 = " << s2 << " N = " << data.size() << endl;
   float min_value = sum - 1.*s2;
   float max_value = sum + 1.*s2;
   //thresh=sum/data.size();
   //if (data.size()==0) thresh=0;
 
   bestThresh = 0;
   bestScore = -FLT_MAX;
   float thresh;
   float score;
   for (thresh = min_value; thresh < max_value; thresh += (max_value-min_value)/15.0)
   {
      int sumAi = 0, sumBi = 0;
      vector<int> Ai (numberClasses, 0);
      vector<int> Bi (numberClasses, 0);
      for (k=0;k<data.size();k++)
      {
         if (data[k].second[dim] >= thresh) 
         {
            sumAi++;
            Ai[data[k].first]++;
         } else {
            sumBi++;
            Bi[data[k].first]++;
         }
      }
      
      double weight = double(sumAi)/data.size();
      score = - numberClasses * .01*abs(thresh-sum)/s2;
      //score = 0;
      for (i = 0;i<numberClasses;i++)
      {
         score += - weight     *  entropy_funct (double( Ai[i] ) / sumAi )
         - (1-weight) *  entropy_funct (double( Bi[i] ) / sumBi );
      }
      if (score > bestScore)
      {
         bestThresh = thresh;
         bestScore = score;
      }
   }
   
}
*/


static int float_less(const void *a, const void *b)
{
   return *((float *)a) < *((float *)b);
}

//find threshold using split at median and mutual information
void Cell::findThreshold(const vector<pair<int, float *> > &data, int dim, float &thresh, float &score)
{
   float sum = 0;
   int i,k;
   if (data.size()==0) thresh=0; 
   else {
      //float sorted[data.size()];
      float *sorted = new float [data.size()];
      for (i=0;i<data.size();i++)
         sorted[i] = data[i].second[dim];
      //qsort(sorted,data.size(),sizeof(float), float_less);
      sort (sorted,sorted+data.size());
      thresh=sorted[data.size()/2];
      delete [] sorted;
   }
   
   int sumAi = 0, sumBi = 0;
   vector<int> Ai (numberClasses, 0);
   vector<int> Bi (numberClasses, 0);
   for (k=0;k<data.size();k++)
   {
      if (data[k].second[dim] >= thresh) 
      {
         sumAi++;
         Ai[data[k].first]++;
      } else {
         sumBi++;
         Bi[data[k].first]++;
      }
   }
   double weight = double(sumAi)/data.size();
   score = 0.0;
   for (i = 0;i<numberClasses;i++)
   {
      score += - weight     *  entropy_funct (double( Ai[i] ) / sumAi )
               - (1-weight) *  entropy_funct (double( Bi[i] ) / sumBi );
   }
   //cerr << score << " " << sumAi << " " << sumBi << " " << weight << " " << Ai[0] << " " << Ai[1] << " " << Bi[0] << " " << Bi[1] << endl;
}


//find threshold using split at average and mutual information
/*void Cell::findThreshold(const vector<pair<int, float *> > &data, int dim, float &thresh, float &score)
{
   float sum = 0;
   int i,k;
   for (i=0;i<data.size();i++)
      sum += data[i].second[dim];
   thresh=sum/data.size();
   if (data.size()==0) thresh=0;

   int sumAi = 0, sumBi = 0;
   vector<int> Ai (numberClasses, 0);
   vector<int> Bi (numberClasses, 0);
   for (k=0;k<data.size();k++)
   {
      if (data[k].second[dim] >= thresh) 
      {
         sumAi++;
         Ai[data[k].first]++;
      } else {
         sumBi++;
         Bi[data[k].first]++;
      }
   }
   
   double weight = double(sumAi)/data.size();
   score = 0.0;
   for (i = 0;i<numberClasses;i++)
   {
      score += - weight     *  entropy_funct (double( Ai[i] ) / sumAi )
               - (1-weight) *  entropy_funct (double( Bi[i] ) / sumBi );
   }

}
*/

/*void Cell::findThreshold(const vector<pair<int, float *> > &data, int dim, float &thresh, float &score)
{
   float sum = 0;
   int i,k;
   for (i=0;i<data.size();i++)
      sum += data[i].second[dim];
   thresh=sum/data.size();
   if (data.size()==0) thresh=0;

   vector<int> scores (numberClasses, 0);
   for (i=0;i<data.size();i++)
      if (data[i].second[dim] >= thresh) 
      {
         scores[data[i].first]++;
      }
      else 
      {
         scores[data[i].first]--;
      }
   score = 0.0;
   for (i=0;i<numberClasses;i++)
   score += abs(scores[i]);

}*/

/*
void Cell::findThreshold(const vector<pair<int, float *> > &data, int dim, float &bestThresh, float &bestScore)
{
   float sum = 0, s2 = 0;
   int i,k;
   for (i=0;i<data.size();i++)
   {
      sum += data[i].second[dim];
      s2+= sqr(data[i].second[dim]);
   }
   if (data.size()<=1)
   {
      bestThresh=0;
      bestScore=0;
      return;
   }
   sum /= data.size();
   s2=sqrt(s2/data.size() - sqr(sum) );
   //cerr << "s2 = " << s2 << " N = " << data.size() << endl;
   float min_value = sum - 1.5*s2;
   float max_value = sum + 1.5*s2;
   //thresh=sum/data.size();
   //if (data.size()==0) thresh=0;

   bestThresh = 0;
   bestScore = -FLT_MAX;
   float thresh;
   float score;
   for (thresh = min_value; thresh < max_value; thresh += (max_value-min_value)/15.0)
   {
      int diff=0;
      //vector<int> scores (numberClasses, 0);
      int binA[numberClasses];
      int binB[numberClasses];
      for (i=0;i<numberClasses;i++)
      {
	 binA[i]=0;
	 binB[i]=0;
      }
      for (i=0;i<data.size();i++)
	 if (data[i].second[dim] >= thresh) 
	 {
	    diff++;
	    binA[data[i].first]++;
	 }
	 else 
	 {
	    diff--;
	    binB[data[i].first]++;
	 }
      score = 0.0;
      for (i=0;i<numberClasses;i++)
	 score += abs(binA[i]-binB[i])/(binA[i]+binB[i]+15);
      score -= .3*numberClasses*abs(diff)/data.size();
      //cerr << .49*numberClasses*abs(diff)/data.size() << endl;
      if (score > bestScore)
      {
         bestThresh = thresh;
         bestScore = score;
      }
   }
   
}
*/

int Cell::setNumbering(int start)
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

int Cell::belongs(float *vect) const
{
   if (terminal) return cellID;
   
   if (vect[splitDimension] < threshold) 
      return first->belongs(vect);
   else
      return second->belongs(vect);
}

void Cell::calcTemplate (const vector<float *> &features, vector<int> &templ) const
{
   for (vector<float *>::const_iterator feature = features.begin(); 
        feature < features.end(); feature++)
   {
      //cerr << "(" << (*feature)[0] << "," << (*feature)[1] << "): " << belongs(*feature) << endl;
      templ[belongs(*feature)]++;
   }
}

void Cell::printOn(ostream &out) const
{
   out << "<Cell " << endl;
   out << "<dimension " << dimension << ">" << endl;
   out << "<numberClasses " << numberClasses << ">" << endl;
   out << "<terminal " << terminal << ">" << endl;
   if (terminal)
   {
      out << "<cellID " << cellID << ">" << endl;
   } else {
      out << "<threshold " << threshold << ">" << endl;
      out << "<splitDimension " << splitDimension << ">" << endl;
      out << "<first " << *first << ">" << endl;;
      out << "<second " << *second << ">" << endl;;
   }
   
   out << ">\n";
}

ostream &operator << (ostream &out, const Cell &cell)
{
   cell.printOn(out);
   return out;
}

void Cell::readFrom (istream &in)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      in >> tag;
      if (tag == "dimension") 
         in >> dimension;
      else if (tag == "numberClasses")
         in >> numberClasses;

      else if (tag == "terminal")
         in >> terminal;

      else if (tag == "cellID")
         in >> cellID;

      else if (tag == "threshold")
         in >> threshold;

      else if (tag == "splitDimension")
         in >> splitDimension;

      else if (tag == "first")
      {
         Cell *tmp = new Cell;
         in >> *tmp;
         first = tmp;
      }
      else if (tag == "second")
      {
         Cell *tmp = new Cell;
         in >> *tmp;
         second = tmp;
      } else 
         throw new ParsingException ("Cell::readFrom : unknown argument: " + tag);
      if (!in) throw new ParsingException ("Cell::readFrom : Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") throw new ParsingException ("Cell::readFrom : Parse error: '>' expected ");
   }
   
}

istream &operator >> (istream &in, Cell &cell)
{
   if (!isValidType(in, "Cell")) 
      return in;
   cell.readFrom(in);
   return in;
}
