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

#include <stream.h>
#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>
#include <unistd.h>

class AudioStream;

//DECLARE_NODE(AudioStream)
NODE_INFO(AudioStream,"Signal:Base", "INPUT", "OUTPUT", "LENGTH:ADVANCE:ENCODING")


static short ulaw2linear[256] = 
{-32124, -31100, -30076, -29052, -28028, -27004, -25980, -24956,
 -23932, -22908, -21884, -20860, -19836, -18812, -17788, -16764,
 -15996, -15484, -14972, -14460, -13948, -13436, -12924, -12412,
 -11900, -11388, -10876, -10364,  -9852,  -9340,  -8828,  -8316,
 -7932,  -7676,  -7420,  -7164,  -6908,  -6652,  -6396,  -6140,
 -5884,  -5628,  -5372,  -5116,  -4860,  -4604,  -4348,  -4092,
 -3900,  -3772,  -3644,  -3516,  -3388,  -3260,  -3132,  -3004,
 -2876,  -2748,  -2620,  -2492,  -2364,  -2236,  -2108,  -1980,
 -1884,  -1820,  -1756,  -1692,  -1628,  -1564,  -1500,  -1436,
 -1372,  -1308,  -1244,  -1180,  -1116,  -1052,   -988,   -924,
 -876,   -844,   -812,   -780,   -748,   -716,   -684,   -652,
 -620,   -588,   -556,   -524,   -492,   -460,   -428,   -396,
 -372,   -356,   -340,   -324,   -308,   -292,   -276,   -260, 
 -244,   -228,   -212,   -196,   -180,   -164,   -148,   -132,
 -120,   -112,   -104,    -96,    -88,    -80,    -72,    -64,
 -56,    -48,    -40,    -32,    -24,    -16,     -8,      0,
 32124,  31100,  30076,  29052,  28028,  27004,  25980,  24956,
 23932,  22908,  21884,  20860,  19836,  18812,  17788,  16764,
 15996,  15484,  14972,  14460,  13948,  13436,  12924,  12412,
 11900,  11388,  10876,  10364,   9852,   9340,   8828,   8316,
 7932,   7676,   7420,   7164,   6908,   6652,   6396,   6140,
 5884,   5628,   5372,   5116,   4860,   4604,   4348,   4092,
 3900,   3772,   3644,   3516,   3388,   3260,   3132,   3004,
 2876,   2748,   2620,   2492,   2364,   2236,   2108,   1980,
 1884,   1820,   1756,   1692,   1628,   1564,   1500,   1436,
 1372,   1308,   1244,   1180,   1116,   1052,    988,    924,
 876,    844,    812,    780,    748,    716,    684,    652,
 620,    588,    556,    524,    492,    460,    428,    396, 
 372,    356,    340,    324,    308,    292,    276,    260,
 244,    228,    212,    196,    180,    164,    148,    132,
 120,    112,    104,     96,     88,     80,     72,     64,
 56,     48,     40,     32,     24,     16,      8,      0};

static short alaw2linear[256] =
{-5504,  -5248,  -6016,  -5760,  -4480,  -4224,  -4992,  -4736,
 -7552,  -7296,  -8064,  -7808,  -6528,  -6272,  -7040,  -6784,
 -2752,  -2624,  -3008,  -2880,  -2240,  -2112,  -2496,  -2368,
 -3776,  -3648,  -4032,  -3904,  -3264,  -3136,  -3520,  -3392,
 -22016, -20992, -24064, -23040, -17920, -16896, -19968, -18944,
 -30208, -29184, -32256, -31232, -26112, -25088, -28160, -27136,
 -11008, -10496, -12032, -11520,  -8960,  -8448,  -9984,  -9472,
 -15104, -14592, -16128, -15616, -13056, -12544, -14080, -13568,
 -344,   -328,   -376,   -360,   -280,   -264,   -312,   -296,
 -472,   -456,   -504,   -488,   -408,   -392,   -440,   -424,
 -88,    -72,   -120,   -104,    -24,     -8,    -56,    -40,
 -216,   -200,   -248,   -232,   -152,   -136,   -184,   -168,
 -1376,  -1312,  -1504,  -1440,  -1120,  -1056,  -1248,  -1184,
 -1888,  -1824,  -2016,  -1952,  -1632,  -1568,  -1760,  -1696,
 -688,   -656,   -752,   -720,   -560,   -528,   -624,   -592,
 -944,   -912,  -1008,   -976,   -816,   -784,   -880,   -848,
 5504,   5248,   6016,   5760,   4480,   4224,   4992,   4736,
 7552,   7296,   8064,   7808,   6528,   6272,   7040,   6784,
 2752,   2624,   3008,   2880,   2240,   2112,   2496,   2368,
 3776,   3648,   4032,   3904,   3264,   3136,   3520,   3392,
 22016,  20992,  24064,  23040,  17920,  16896,  19968,  18944,
 30208,  29184,  32256,  31232,  26112,  25088,  28160,  27136,
 11008,  10496,  12032,  11520,   8960,   8448,   9984,   9472,
 15104,  14592,  16128,  15616,  13056,  12544,  14080,  13568,
 344,    328,    376,    360,    280,    264,    312,    296,
 472,    456,    504,    488,    408,    392,    440,    424,
 88,     72,    120,    104,     24,      8,     56,     40,
 216,    200,    248,    232,    152,    136,    184,    168,
 1376,   1312,   1504,   1440,   1120,   1056,   1248,   1184,
 1888,   1824,   2016,   1952,   1632,   1568,   1760,   1696,
 688,    656,    752,    720,    560,    528,    624,    592,
 944,    912,   1008,    976,    816,    784,    880,    848};





class AudioStream : public FrameOperation {
   
   typedef enum {ulaw, alaw, lin8, lin16} EncodeType;
   typedef enum {fd, fptr, cpp} StreamType;
   
   int inputID;
   int advance;

   StreamType strType;
   
   /**Encoding type for samples*/
   EncodeType encoding;

   /**Sample size (in bytes) either 1 or 2*/
   int itemSize;

   /**Internal temporary variable used when converting sample to float*/
   char *tmpBuffer;

public:
   AudioStream(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      inputID = addInput("INPUT");
      if (parameters.exist("ADVANCE"))
         advance = dereference_cast<int> (parameters.get("ADVANCE"));
      else advance = outputLength;

      setEncoding(object_cast<String> (parameters.get("ENCODING")));

      strType = cpp;
      if (parameters.exist("STREAM_TYPE"))
      {
	 if (object_cast<String> (parameters.get("STREAM_TYPE")) == "fd")
	    strType = fd;
	 else if (object_cast<String> (parameters.get("STREAM_TYPE")) == "FILE")
	    strType = fptr;
	 else if (object_cast<String> (parameters.get("STREAM_TYPE")) == "stream")
	    strType = cpp;
      }
   }

   ~AudioStream() {delete [] tmpBuffer;}

   void setEncoding(const string &enc)
   {
      if (enc == "ULAW")
	 encoding = ulaw;
      else if (enc == "ALAW")
	 encoding = alaw;
      else if (enc == "LIN8")
      encoding = lin8;
      else if (enc == "LIN16")
	 encoding = lin16;
      else 
	 throw NodeException(this, string("Invalid encoding: ") + enc,__FILE__, __LINE__);
      itemSize = encoding == lin16 ? 2 : 1;
      tmpBuffer = new char [itemSize*advance];
   }
   
   virtual void specificInitialize()
   {
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      NodeInput input = inputs[inputID];
      ObjectRef inputValue = input.node->getOutput(input.outputID, 0);

      for (int i=processCount+1;i<=count;i++)
      {	 
	 Vector<float> &output = object_cast<Vector<float> > (out[i]);
	 if (inputValue->status != Object::valid)
	 {
	    output.status = inputValue->status;
	    continue;
	 }
	 

	 if (0 && i>0 && out[i-1]->status == Object::valid 
	     && advance < outputLength)
	 {
	    Vector<float> &previous = object_cast<Vector<float> > (out[i-1]);
	    for (int i=0;i<outputLength-advance;i++)
	       output[i]=previous[i+advance];
	 } else {
	    for (int i=0;i<outputLength-advance;i++)  
	       output[i]=0;
	 }

	 if (strType == cpp)
	 {
	    IStream &file = object_cast<IStream> (inputValue);
	    file.read(tmpBuffer,itemSize*advance);
	    if (file.eof())
	       output.status = Object::past_end;
	 }
	 else if (strType == fptr)
	 {
	    FILE *file = dereference_cast<FILEPTR> (inputValue);
	    fread (tmpBuffer, 1, itemSize*advance, file);
	 } else if (strType == fd)
	 {
	    int file = dereference_cast<int> (inputValue);
	    read (file, tmpBuffer, itemSize*advance);
	 }
	 raw2Float (tmpBuffer, output.end() - advance, advance, encoding);

	 output.status = Object::valid;
      }
   }

   protected:
 void raw2Float (void *in, float *out, int length, EncodeType encoding)
{
   int i;
   switch (encoding)
   {
   case lin16:
      for (i=0;i<length;i++)
         out[i]=(float) (((short *)in)[i]);
      break;
   case ulaw:
      for (i=0;i<length;i++)
         out[i]=(float) ulaw2linear[((unsigned char *)in)[i]];
      break;
   case alaw:
      for (i=0;i<length;i++)
         out[i]=(float) alaw2linear[((unsigned char *)in)[i]];
      break;
   case lin8:
      for (i=0;i<length;i++)
         out[i]=(float) (((unsigned char *)in)[i]);
      break;
   default:
      throw NodeException(this, "Unimplemented encoding type",__FILE__, __LINE__);
   }
}
};
