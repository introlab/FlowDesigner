// Copyright (C) 2001 Jean-Marc Valin

#include "DiagGMM.h"
#include "misc.h"
#include "vec.h"
#include "ObjectParser.h"
#include "binio.h"
#include "iextensions.h"

DECLARE_TYPE(DiagGMM)
//@implements DGMM


#ifdef _ALLOW_SSE
inline float mahalanobis4_SSE(const float *a, const float *b, const float *c, int len)
{
   float sum=0;
   __asm__ __volatile__ (
   "
   push %%eax
   push %%esi
   push %%edi
   push %%ecx
   xorps %%xmm4, %%xmm4
   xorps %%xmm5, %%xmm5

   sub $8, %%ecx
   jb .mul8_skip%=

   prefetcht0 (%%esi)
   prefetcht0 (%%edi)
.mul8_loop%=:
   movaps (%%eax), %%xmm0
   movaps (%%edi), %%xmm1
   movaps 16(%%eax), %%xmm2
   movaps 16(%%edi), %%xmm3
   movaps (%%esi), %%xmm6
   movaps 16(%%esi), %%xmm7
   add $32, %%eax
   add $32, %%edi
   add $32, %%esi
   prefetcht0 (%%esi)
   prefetcht0 (%%edi)
   subps %%xmm0, %%xmm1
   subps %%xmm2, %%xmm3
   mulps %%xmm1, %%xmm1
   mulps %%xmm3, %%xmm3
   mulps %%xmm6, %%xmm1
   mulps %%xmm7, %%xmm3
   addps %%xmm1, %%xmm4
   addps %%xmm3, %%xmm5

   sub $8,  %%ecx
   jae .mul8_loop%=

.mul8_skip%=:
   addps %%xmm5, %%xmm4


   add $4, %%ecx
   jl .mul4_skip%=

   movaps (%%eax), %%xmm0
   movaps (%%edi), %%xmm1
   movaps (%%esi), %%xmm6
   add $16, %%eax
   add $16, %%edi
   add $16, %%esi

   subps %%xmm0, %%xmm1
   mulps %%xmm1, %%xmm1
   mulps %%xmm6, %%xmm1
   addps %%xmm1, %%xmm4

   sub $4,  %%ecx

.mul4_skip%=:



   movaps %%xmm4, %%xmm3


   movhlps %%xmm3, %%xmm4
   addps %%xmm4, %%xmm3
   movaps %%xmm3, %%xmm4
   shufps $33, %%xmm4, %%xmm4
   addss %%xmm4, %%xmm3
   movss %%xmm3, (%%edx)


   pop %%ecx
   pop %%edi
   pop %%esi
   pop %%eax
   "
   : : "a" (a), "S" (c), "D" (b), "c" (len), "d" (&sum)
   : "memory"
   );
    
   return sum;
}
#else
inline float mahalanobis4_SSE<float>(const float *a, const float *b, const float *c, int len)
{
   throw new generalException("Trying to use SSE routine, but code not compiled for SSE support", __FILE__,
			      __LINE__);
}
#endif


float DiagGMM::score(const float *vec)
{
   float score;
   DYN_VEC(char, augDim*sizeof(float)+CACHE_LINES, mem);
   //char mem[augDim*sizeof(float)+CACHE_LINES];
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

   if (IExtensions::haveSSE())
   {
      for (int k=0;k<nbGauss;k++)
      {
	 score = 0;
	 score = mahalanobis4_SSE(data, mean, cov, augDim);
	 if (k==0 || score > maxScore)
	    maxScore = score;
	 mean+=inc;
	 cov += inc;
      }
   } else {
      for (int k=0;k<nbGauss;k++)
      {
	 score = 0;
	 score = vec_mahalanobis2(data, mean, cov, augDim);
	 if (k==0 || score > maxScore)
	    maxScore = score;
	 mean+=inc;
	 cov += inc;
      }
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

	    for (int j=dim+1;j<augDim;j++)
	    {
	       mean[j] = 0;
	       cov[j] = 0;
	    }

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
      for (int j=dim+1;j<augDim;j++)
      {
	 mean[j] = 0;
	 cov[j] = 0;
      }
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
