// Copyright (C) 1999 Jean-Marc Valin


#include "RBF.h"
#include "ObjectParser.h"
#include "misc.h"

using namespace std;

namespace FD {

DECLARE_TYPE(RBF)
//@implements VQ

void RBF::train (int codeSize, const vector<float *> &data/*, const vector<float *> &data_out*/, int len, bool binary)
{
   KMeans::train (codeSize, data, len, binary);

   covar.resize(nbClasses());

   vector<int> counts(covar.size(), 0);
   for (size_t i=0;i<covar.size();i++)
      covar[i].resize(len,0);
   for (size_t i=0;i<data.size();i++)
   {
      int id = getClassID(data[i]);
      counts[id]++;
      for (int j=0;j<len;j++)
	 covar[id][j] += sqr(data[i][j] - means[id][j]);
   }

   for (size_t i=0;i<covar.size();i++)
   {
      float scale = 1.0/counts[i];
      for (int j=0;j<len;j++)
	 covar[i][j] = 1/(covar[i][j]*scale);
   }

   //Calculate X'X and X'Y
   //invert matrix

}

void RBF::calcDist (const float *v, float *dist_return) const
{
   for (size_t i=0;i<means.size();i++)
   {
      dist_return[i] = mahalanobis(&means[i][0], &covar[i][0], v, length);
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
       throw new ParsingException ("RBF::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "length")
         in >> length;
      else if (tag == "covar")
         in >> covar;
      else if (tag == "means")
         in >> means;
      else
         throw new ParsingException ("RBF::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("RBF::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("RBF::readFrom : Parse error: '>' expected ");
   }
}

istream &operator >> (istream &in, RBF &mdl)
{
   if (!isValidType(in, "RBF")) return in;
   mdl.readFrom(in);
   return in;
}
}//namespace FD
