// Copyright (C) 2001 Jean-Marc Valin

#include "DiagGMM.h"
#include "misc.h"
#include "vec.h"
#include "ObjectParser.h"
#include "binio.h"
#include "iextensions.h"

DECLARE_TYPE(DiagGMM)
//@implements DGMM


#ifdef _ENABLE_SSE

#ifdef __GNUC__
inline float mahalanobis4_SSE(const float *a, const float *b, const float *c, int len)
{
   float sum=0;
   __asm__ __volatile__ (
   "\tpush %%eax\n"
   "\tpush %%esi\n"
   "\tpush %%edi\n"
   "\tpush %%ecx\n"
   "\txorps %%xmm4, %%xmm4\n"
   "\txorps %%xmm5, %%xmm5\n"

   "\tsub $8, %%ecx\n"
   "\tjb .mul8_skip%=\n"

   "\tprefetcht0 (%%esi)\n"
   "\tprefetcht0 (%%edi)\n"
".mul8_loop%=:\n"
   "\tmovaps (%%eax), %%xmm0\n"
   "\tmovaps (%%edi), %%xmm1\n"
   "\tmovaps 16(%%eax), %%xmm2\n"
   "\tmovaps 16(%%edi), %%xmm3\n"
   "\tmovaps (%%esi), %%xmm6\n"
   "\tmovaps 16(%%esi), %%xmm7\n"
   "\tadd $32, %%eax\n"
   "\tadd $32, %%edi\n"
   "\tadd $32, %%esi\n"
   "\tprefetcht0 (%%esi)\n"
   "\tprefetcht0 (%%edi)\n"
   "\tsubps %%xmm0, %%xmm1\n"
   "\tsubps %%xmm2, %%xmm3\n"
   "\tmulps %%xmm1, %%xmm1\n"
   "\tmulps %%xmm3, %%xmm3\n"
   "\tmulps %%xmm6, %%xmm1\n"
   "\tmulps %%xmm7, %%xmm3\n"
   "\taddps %%xmm1, %%xmm4\n"
   "\taddps %%xmm3, %%xmm5\n"

   "\tsub $8,  %%ecx\n"
   "\tjae .mul8_loop%=\n"

".mul8_skip%=:\n"
   "\taddps %%xmm5, %%xmm4\n"


   "\tadd $4, %%ecx\n"
   "\tjl .mul4_skip%=\n"

   "\tmovaps (%%eax), %%xmm0\n"
   "\tmovaps (%%edi), %%xmm1\n"
   "\tmovaps (%%esi), %%xmm6\n"
   "\tadd $16, %%eax\n"
   "\tadd $16, %%edi\n"
   "\tadd $16, %%esi\n"

   "\tsubps %%xmm0, %%xmm1\n"
   "\tmulps %%xmm1, %%xmm1\n"
   "\tmulps %%xmm6, %%xmm1\n"
   "\taddps %%xmm1, %%xmm4\n"

   "\tsub $4,  %%ecx\n"

".mul4_skip%=:\n"



   "\tmovaps %%xmm4, %%xmm3\n"


   "\tmovhlps %%xmm3, %%xmm4\n"
   "\taddps %%xmm4, %%xmm3\n"
   "\tmovaps %%xmm3, %%xmm4\n"
   "\tshufps $0x55, %%xmm4, %%xmm4\n"
   "\taddss %%xmm4, %%xmm3\n"
   "\tmovss %%xmm3, (%%edx)\n"


   "\tpop %%ecx\n"
   "\tpop %%edi\n"
   "\tpop %%esi\n"
   "\tpop %%eax\n"
   : : "a" (a), "S" (c), "D" (b), "c" (len), "d" (&sum)
   : "memory"
   );
    
   return sum;
}
#elif defined(WIN32)


inline float mahalanobis4_SSE(const float *a, const float *b, const float *c, int len)
{
   float sum=0;
_asm {
   push eax
   push esi
   push edi
   push ecx
   push edx
   

   mov eax, a
   mov esi, c
   mov edi, b
   mov ecx, len
   lea edx, sum
   xorps xmm4, xmm4
   xorps xmm5, xmm5

   sub ecx, 8
   jb mul8_skip

   prefetcht0 [esi]
   prefetcht0 [edi]
mul8_loop:
   movaps xmm0, [eax]
   movaps xmm1, [edi]
   movaps xmm2, 16[eax]
   movaps xmm3, 16[edi]
   movaps xmm6, [esi]
   movaps xmm7, 16[esi]
   add eax, 32
   add edi, 32
   add esi, 32
   prefetcht0 [esi]
   prefetcht0 [edi]
   subps xmm1, xmm0
   subps xmm3, xmm2
   mulps xmm1, xmm1
   mulps xmm3, xmm3
   mulps xmm1, xmm6
   mulps xmm3, xmm7
   addps xmm4, xmm1
   addps xmm5, xmm3

   sub  ecx, 8
   jae mul8_loop

mul8_skip:
   addps xmm4, xmm5


   add ecx, 4
   jl mul4_skip

   movaps xmm0, [eax]
   movaps xmm1, [edi]
   movaps xmm6, [esi]
   add eax, 16
   add edi, 16
   add esi, 16

   subps xmm1, xmm0
   mulps xmm1, xmm1
   mulps xmm1, xmm6
   addps xmm4, xmm1

   sub  ecx, 4

mul4_skip:



   movaps xmm3, xmm4


   movhlps xmm4, xmm3
   addps xmm3, xmm4
   movaps xmm4, xmm3
   shufps xmm4, xmm4, 0x55
   addss xmm3, xmm4
   movss [edx], xmm3


   pop edx
   pop ecx
   pop edi
   pop esi
   pop eax
   }    
   return sum;
}


#endif /*WIN32*/

#else
inline float mahalanobis4_SSE(const float *a, const float *b, const float *c, int len)
{
   throw new GeneralException("Trying to use SSE routine, but code not compiled for SSE support", __FILE__,
			      __LINE__);
}
#endif /*_ENABLE_SSE*/


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
       throw new ParsingException ("DiagGMM::readFrom : Parse error: '<' expected");
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
         throw new ParsingException ("DiagGMM::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("DiagGMM::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("DiagGMM::readFrom : Parse error: '>' expected ");
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
