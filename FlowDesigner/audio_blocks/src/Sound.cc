// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau
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


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <stream.h>
#include <strstream.h>

#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/soundcard.h>
#include <unistd.h>

class Sound;

//DECLARE_NODE(Sound)
NODE_INFO(Sound,"Signal:Audio", "", "OUTPUT", "DEVICE:RATE:STEREO:MODE:BUFFER:DUMMY")

/** A constant node contains a value that will never changes. */
class Sound : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /*the file descriptor*/
   int audio_fd;
   /**The ID of the 'value' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   Sound(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      //, value (parameters.get("VALUE"))
   {
      outputID = addOutput("OUTPUT");
      
   }

   void specificInitialize()
   {
      Node::specificInitialize();

      int speed=44100;
      int stereo=0;
      //int audio_fd;
      String device = object_cast <String> (parameters.get("DEVICE"));
      if (parameters.exist("RATE"))
	 speed = dereference_cast<int> (parameters.get("RATE"));
      if (parameters.exist("STEREO"))
	 stereo = dereference_cast<int> (parameters.get("STEREO")); 
      int mode=O_WRONLY;
      if (parameters.exist("DUMMY"))
	 mode |= O_CREAT;
      if (parameters.exist("MODE"))
      {
	 String modeStr = object_cast <String> (parameters.get("MODE"));
	 if (modeStr == "R")
	    mode=O_RDONLY;
	 if (modeStr == "RW")
	    mode=O_RDWR;
      }
      if ((audio_fd=open(device.c_str(),mode)) == -1) 
      {
	 perror (device.c_str());
	 //close(audio_fd);
	 throw new NodeException(NULL, "Can't open sound device\n", __FILE__, __LINE__);
	 //exit(1);
      }
      if (!parameters.exist("DUMMY"))
      {
      //int arg=0x7fff0004;
      int arg=0x0004000a;
      if (parameters.exist("BUFFER"))
      {
	 unsigned int buffLen = dereference_cast<int> (parameters.get("BUFFER"));
	 buffLen--;
	 arg=-1;
	 while (buffLen)
	 {
	    arg++;
	    buffLen >>= 1;
	 }
	 if (arg < 4)
	    arg=4;
	 arg |= 0x00040000;
	 //cerr << "arg = " << arg << endl;
      }      
      ioctl(audio_fd, SNDCTL_DSP_SETFRAGMENT, &arg);
      

      int format=AFMT_S16_LE;
      if (ioctl(audio_fd, SNDCTL_DSP_SETFMT, &format)==-1)
      {
	 perror("SNDCTL_DSP_SETFMT");
	 close(audio_fd);
	 throw new NodeException(NULL, "Can't set the right format\n", __FILE__, __LINE__);
      }
      
      if (ioctl(audio_fd, SNDCTL_DSP_STEREO, &stereo)==-1)
      {
	 perror("SNDCTL_DSP_STEREO");
	 close(audio_fd);
	 throw new NodeException(NULL, "Can't set/reset stereo mode\n", __FILE__, __LINE__);
      }
      
      if (ioctl(audio_fd, SNDCTL_DSP_SPEED, &speed)==-1)
      {
	 perror("SNDCTL_DSP_SPEED");
	 close(audio_fd);
	 throw new NodeException(NULL, "Can't set sound device speed\n", __FILE__, __LINE__);
      }
      }

      value = ObjectRef(new Int(audio_fd));
      //Vector<float> &val = object_cast<Vector<float> > (value);
      //istrstream str_vector(object_cast <String> (parameters.get("VALUE")).c_str());
      //str_vector >> val;

      //cerr << "vector is: " << val << endl;
      
   }
      
   virtual ~Sound()
   {
      //cerr << "Sound destructor\n";
      close(audio_fd);
   }
   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw new NodeException (this, "Sound: Unknown output id", __FILE__, __LINE__);
   }

protected:
   /**Default constructor, should not be used*/
   Sound() {throw new GeneralException("Sound copy constructor should not be called",__FILE__,__LINE__);}

};
