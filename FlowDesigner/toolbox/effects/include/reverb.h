// Copyright (C) 2001 Jean-Marc Valin

// Written by Jezar at Dreampoint, June 2000
// http://www.dreampoint.co.uk
// This code is public domain

#ifndef REVERB_H
#define REVERB_H

namespace FD {

#define undenormalise(sample) if(((*(unsigned int*)&sample)&0x7f800000)==0) sample=0.0f


class allpass
{
  public:
   allpass();
   void	setbuffer(float *buf, int size);
   inline  float	process(float inp);
   void	mute();
   void	setfeedback(float val);
   float	getfeedback();
// private:
   float	feedback;
   float	*buffer;
   int		bufsize;
   int		bufidx;
};


// Big to inline - but crucial for speed

inline float allpass::process(float input)
{
   float output;
   float bufout;
	
   bufout = buffer[bufidx];
   undenormalise(bufout);
	
   output = -input + bufout;
   buffer[bufidx] = input + (bufout*feedback);

   if(++bufidx>=bufsize) bufidx = 0;

   return output;
}


class comb
{
  public:
   comb();
   void	setbuffer(float *buf, int size);
   inline  float	process(float inp);
   void	mute();
   void	setdamp(float val);
   float	getdamp();
   void	setfeedback(float val);
   float	getfeedback();
  private:
   float	feedback;
   float	filterstore;
   float	damp1;
   float	damp2;
   float	*buffer;
   int		bufsize;
   int		bufidx;
};


// Big to inline - but crucial for speed

inline float comb::process(float input)
{
   float output;

   output = buffer[bufidx];
   undenormalise(output);

   filterstore = (output*damp2) + (filterstore*damp1);
   undenormalise(filterstore);

   buffer[bufidx] = input + (filterstore*feedback);

   if(++bufidx>=bufsize) bufidx = 0;

   return output;
}





const int	numcombs		= 8;
const int	numallpasses	= 4;
const float	muted			= 0;
const float	fixedgain		= 0.015f;
const float scalewet		= 3;
const float scaledry		= 2;
const float scaledamp		= 0.4f;
const float scaleroom		= 0.28f;
const float offsetroom		= 0.7f;
const float initialroom		= 0.5f;
const float initialdamp		= 0.5f;
const float initialwet		= 1/scalewet;
const float initialdry		= 0;
const float initialwidth	= 1;
const float initialmode		= 0;
const float freezemode		= 0.5f;
const int	stereospread	= 23;

// These values assume 44.1KHz sample rate
// they will probably be OK for 48KHz sample rate
// but would need scaling for 96KHz (or other) sample rates.
// The values were obtained by listening tests.
const int combtuningL1		= 1116;
const int combtuningR1		= 1116+stereospread;
const int combtuningL2		= 1188;
const int combtuningR2		= 1188+stereospread;
const int combtuningL3		= 1277;
const int combtuningR3		= 1277+stereospread;
const int combtuningL4		= 1356;
const int combtuningR4		= 1356+stereospread;
const int combtuningL5		= 1422;
const int combtuningR5		= 1422+stereospread;
const int combtuningL6		= 1491;
const int combtuningR6		= 1491+stereospread;
const int combtuningL7		= 1557;
const int combtuningR7		= 1557+stereospread;
const int combtuningL8		= 1617;
const int combtuningR8		= 1617+stereospread;
const int allpasstuningL1	= 556;
const int allpasstuningR1	= 556+stereospread;
const int allpasstuningL2	= 441;
const int allpasstuningR2	= 441+stereospread;
const int allpasstuningL3	= 341;
const int allpasstuningR3	= 341+stereospread;
const int allpasstuningL4	= 225;
const int allpasstuningR4	= 225+stereospread;

class revmodel
{
  public:
   revmodel();
   void	mute();
   void	processmix(float *inputL, float *inputR, float *outputL, float *outputR, long numsamples, int skip);
   void	processreplace(const float *inputL, const float *inputR, float *outputL, float *outputR, long numsamples, int skip);
   void	setroomsize(float value);
   float	getroomsize();
   void	setdamp(float value);
   float	getdamp();
   void	setwet(float value);
   float	getwet();
   void	setdry(float value);
   float	getdry();
   void	setwidth(float value);
   float	getwidth();
   void	setmode(float value);
   float	getmode();
  private:
   void	update();
  private:
   float	gain;
   float	roomsize,roomsize1;
   float	damp,damp1;
   float	wet,wet1,wet2;
   float	dry;
   float	width;
   float	mode;

   // The following are all declared inline 
   // to remove the need for dynamic allocation
   // with its subsequent error-checking messiness

   // Comb filters
   comb	combL[numcombs];
   comb	combR[numcombs];

   // Allpass filters
   allpass	allpassL[numallpasses];
   allpass	allpassR[numallpasses];

   // Buffers for the combs
   float	bufcombL1[combtuningL1];
   float	bufcombR1[combtuningR1];
   float	bufcombL2[combtuningL2];
   float	bufcombR2[combtuningR2];
   float	bufcombL3[combtuningL3];
   float	bufcombR3[combtuningR3];
   float	bufcombL4[combtuningL4];
   float	bufcombR4[combtuningR4];
   float	bufcombL5[combtuningL5];
   float	bufcombR5[combtuningR5];
   float	bufcombL6[combtuningL6];
   float	bufcombR6[combtuningR6];
   float	bufcombL7[combtuningL7];
   float	bufcombR7[combtuningR7];
   float	bufcombL8[combtuningL8];
   float	bufcombR8[combtuningR8];

   // Buffers for the allpasses
   float	bufallpassL1[allpasstuningL1];
   float	bufallpassR1[allpasstuningR1];
   float	bufallpassL2[allpasstuningL2];
   float	bufallpassR2[allpasstuningR2];
   float	bufallpassL3[allpasstuningL3];
   float	bufallpassR3[allpasstuningR3];
   float	bufallpassL4[allpasstuningL4];
   float	bufallpassR4[allpasstuningR4];
};

}//namespace FD

#endif
