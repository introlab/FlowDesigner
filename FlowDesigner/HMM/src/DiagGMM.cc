// Copyright (C) 2001 Jean-Marc Valin

#include "DiagGMM.h"
#include "misc.h"
#include "vec.h"
#include "ObjectParser.h"
#include "binio.h"

DECLARE_TYPE(DiagGMM)

float DiagGMM::score(const float *vec)
{
   float score;
   char mem[augDim*sizeof(float)+CACHE_LINES];
   //cerr << augDim << endl;
   float *data = (float *) (((unsigned long)(mem) + (CACHE_LINES-1))&CACHE_MASK);
   for (int i=0;i<dim;i++)
      data[i]=vec[i];
   data[dim]=1;
   for (int i=dim+1;i<augDim;i++)
      data[i]=0;

   int inc=2*augDim;
   float *mean=base;
   float *cov=base+augDim;
   float maxScore=0;
   for (int k=0;k<nbGauss;k++)
   {
      score = 0;
      //for (int i=0;i<augDim;i++)
      // score += sqr(data[i]-mean[i])*cov[i];
      score = vec_mahalanobis2(data, mean, cov, augDim);
      //cerr << score << endl;
      if (k==0 || score > maxScore)
	 maxScore = score;
      mean+=inc;
      cov += inc;
   }
   //cerr << "tata\n";
   return maxScore;
}


void DiagGMM::printOn(ostream &out) const
{
   out << "<DiagGMM " << endl;
   out << "<nbGauss " << nbGauss << ">" << endl;
   out << "<dimensions " << dim << ">" << endl;
   out << "<data ";
   int inc=2*augDim;
   float *mean=base;
   float *cov=base+augDim;
   for (int i=0;i<nbGauss;i++)
   {
      for (int j=0;j<dim+1;j++)
	 out << mean[j] << " ";
      for (int j=0;j<dim+1;j++)
	 out << cov[j] << " ";
      out << endl;
      mean+=inc;
      cov += inc;
   }
   out << ">\n";
   out << ">\n";
}


void DiagGMM::readFrom (istream &in)
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
      if (tag == "nbGauss")
         in >> nbGauss;
      else if (tag == "dimensions")
      {
         in >> dim;
	 augDim = (dim+4)&0xfffffffc;
      }
      else if (tag == "data")
      {
	 int allocSize = 2 * augDim * nbGauss * sizeof(float)  +  CACHE_LINES;
	 ptr = new char [allocSize];
	 base = (float *) (((unsigned long)(ptr) + (CACHE_LINES-1))&CACHE_MASK);
	 
	 int inc=2*augDim;
	 float *mean=base;
	 float *cov=base+augDim;
	 for (int i=0;i<nbGauss;i++)
	 {
	    for (int j=0;j<dim+1;j++)
	       in >> mean[j];
	    for (int j=0;j<dim+1;j++)
	       in >> cov[j];
	    mean+=inc;
	    cov += inc;
	 }
      } 
      else
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
}

void DiagGMM::serialize(ostream &out) const
{
   out << "{DiagGMM " << endl;
   out << "|";
   BinIO::write(out, &nbGauss, 1);
   BinIO::write(out, &dim, 1);

   int inc=2*augDim;
   float *mean=base;
   float *cov=base+augDim;
   for (int i=0;i<nbGauss;i++)
   {
      BinIO::write(out, mean, dim+1);
      BinIO::write(out, cov, dim+1);
      mean+=inc;
      cov += inc;
   }
   out << "}" << endl;
}

void DiagGMM::unserialize(istream &in)
{
   BinIO::read(in, &nbGauss, 1);
   BinIO::read(in, &dim, 1);

   augDim = (dim+4)&0xfffffffc;
   int allocSize = 2 * augDim * nbGauss * sizeof(float)  +  CACHE_LINES;
   ptr = new char [allocSize];
   base = (float *) (((unsigned long)(ptr) + (CACHE_LINES-1))&CACHE_MASK);
   int inc=2*augDim;
   float *mean=base;
   float *cov=base+augDim;
   for (int i=0;i<nbGauss;i++)
   {
      BinIO::read(in, mean, dim+1);
      BinIO::read(in, cov, dim+1);
      mean+=inc;
      cov += inc;
   }
   
}


istream &operator >> (istream &in, DiagGMM &gmm)
{
   if (!isValidType(in, "DiagGMM")) return in;
   gmm.readFrom(in);
   return in;
}
