// Copyright (C) 1999 Jean-Marc Valin

#include <iostream>
#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

#ifndef WIN32
#include <unistd.h>
#endif
#include "Stream.h"

using namespace std;

class AudioStream;

DECLARE_NODE(AudioStream)
/*Node
 *
 * @name AudioStream
 * @category DSP:Audio
 * @description Reads an audio stream and outputs frames
 *
 * @input_name INPUT
 * @input_type Stream
 * @input_description An audio input stream (IStream, fd or FILE *)
 *
 * @output_name AUDIO
 * @output_type Vector<float>
 * @output_description Frames read
 *
 * @output_name NOT_EOF
 * @output_type bool
 * @output_description True if we haven't reach the end of file yet
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Length of the frames (in samples)
 *
 * @parameter_name ADVANCE
 * @parameter_type int
 * @parameter_description Offset beween frames (in samples)
 *
 * @parameter_name ENCODING
 * @parameter_type string
 * @parameter_description Type of encoding (LIN16, ULAW, ALAW, LIN8, SPHERE)
 *
 * @parameter_name STREAM_TYPE
 * @parameter_type string
 * @parameter_description Type of stream (stream, fd, FILE)
 *
 * @parameter_name REWIND
 * @parameter_type bool
 * @parameter_description If true, the stream rewinds to the beginning of the file when EOF is met
 *
END*/



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





class AudioStream : public BufferedNode {
   
   typedef enum {ulaw, alaw, lin8, lin16, sphere} EncodeType;
   typedef enum {fd, fptr, cpp} StreamType;
   
   int inputID;
   int audioID;
   int eofID;
   int outputLength;
   int advance;

   StreamType strType;
   
   /**Encoding type for samples*/
   EncodeType encoding;

   /**Sample size (in bytes) either 1 or 2*/
   int itemSize;

   /**Internal temporary variable used when converting sample to float*/
   vector<char> tmpBuffer;

   bool rewind;
public:
   AudioStream(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      audioID = addOutput("AUDIO");
      eofID = addOutput("NOT_EOF");

      if (parameters.exist("OUTPUTLENGTH"))
         outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
      else outputLength = dereference_cast<int> (parameters.get("LENGTH"));
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

      if (parameters.exist("REWIND"))
      {
	 rewind = dereference_cast<bool> (parameters.get("REWIND"));
      } else
	 rewind = false;

      inOrder = true;
   }

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
      else if (enc == "SPHERE" || enc == "NIST")
	 encoding = sphere;
      else 
	 throw new NodeException(NULL, string("Invalid encoding: ") + enc,__FILE__, __LINE__);
      itemSize = encoding == lin16 ? 2 : 1;
      tmpBuffer.resize(itemSize*advance);
   }
   
   virtual void initialize()
   {
      outputs[audioID].lookAhead = outputs[eofID].lookAhead = 1+max(outputs[audioID].lookAhead, outputs[eofID].lookAhead);
      //outputs[audioID].lookBack += 1;
      this->BufferedNode::initialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      Buffer &audioBuffer = *(outputs[audioID].buffer);
      Buffer &eofBuffer = *(outputs[eofID].buffer);
      eofBuffer[count] = TrueObject;

      ObjectRef inputValue = getInput(inputID, count);

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      audioBuffer[count] = &output;
      
      
      //Done only the first time
      if (count == 0)
      {
	 //If encoding is sphere, skip header
	 if (encoding == sphere)
	 {
	    char head[16];
	    if (!readStream(head, 16, inputValue))
	    {
	       throw new NodeException(this, "Looks like an empty file", __FILE__, __LINE__);
	    }
	    head[15]=0;
	    int len = atoi(head+11);
	    len -= 16;
	    // Skip rest of header
	    while (len > 0)
	    {
	       char dummy[1024];
	       int skip = min (len, 1024);
	       if (!readStream(dummy, skip, inputValue))
	       {
		  throw new NodeException(this, "Truncated SPHERE file", __FILE__, __LINE__);
	       }
	       len -= skip;
	    }
	 }

	 // Read whole frame for the first time
	 //char buff[itemSize*outputLength];
	 DYN_VEC(char, itemSize*outputLength, buff);
	 if (!readStream(buff, outputLength, inputValue))
	 {
	    eofBuffer[count] = FalseObject;
	    for (int i=0;i<outputLength;i++)
	       output[i] = 0;
	 } else {
	    //FIXME: We should only replace unavailable data by zero, not the whole frame
	    raw2Float (buff, &output[0], outputLength, encoding);
	 }
	 
      } else { // Normal case (not at start of file)
      
	 if (advance < outputLength)
	 {
	    Vector<float> &previous = object_cast<Vector<float> > (audioBuffer[count-1]);
	    for (int i=0;i<outputLength-advance;i++)
	       output[i]=previous[i+advance];
	 } else {
	    //FIXME: should skip data if (advance > outputLength)
	    for (int i=0;i<outputLength-advance;i++)  
	       output[i]=0;
	 }
	 
	 
	 if (!readStream(&tmpBuffer[0], advance, inputValue))
	 {
	    eofBuffer[count] = FalseObject;
	    //FIXME: We should only replace unavailable data by zero, not the whole frame
	    for (int i=0;i<outputLength;i++)
	       output[i] = 0;
	 } else {
	    int convert = min(advance, outputLength);
	    int outSz = output.size();
	    raw2Float (&tmpBuffer[0], &output[outSz] - convert, convert, encoding);
	 }
      }

      
   }

protected:
   /*Converts raw samples into float*/
   void raw2Float (void *in, float *out, int length, EncodeType encoding)
   {
      int i;
      switch (encoding)
      {
	 case lin16:
	    for (i=0;i<length;i++)
	       out[i]=(float) (((short *)in)[i]);
	    break;
	 case sphere:
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
	    throw new NodeException(this, "Unimplemented encoding type",__FILE__, __LINE__);
      }
   }

   bool readStream(char *buffer, int length, ObjectRef inputValue)
   {
      if (strType == cpp)
      {
	 IStream &file = object_cast<IStream> (inputValue);
	 file.read(buffer,itemSize*length);
	 if (file.eof())
	 {
	    //FIXME: Should be implemented for other streams and should check for SPHERE header
	    if (rewind)
	    {
	       file.seekg(0, ios::beg);
	       file.read(buffer,itemSize*length);
	       istream &istr=file;
	       istr.clear();
	    } else
	       return false;
	 }
      } else if (strType == fptr)
      {
	 FILE *file = dereference_cast<FILE *> (inputValue);
	 fread (buffer, 1, itemSize*length, file);
	 if (feof(file))
	 {
	    return false;
	 }
      } 
#ifndef WIN32
      else if (strType == fd)
      {
	 int file = dereference_cast<int> (inputValue);
	 if (read (file, buffer, itemSize*length) != itemSize*length)
	 {
	    return false;
	 }
      }
#endif
      return true;
   }

};
