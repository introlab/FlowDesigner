// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>

#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include "stream_wrap.h"
#include "Stream.h"

#ifdef HAVE_MACHINE_SOUNDCARD_H
#define HAVE_OSS_AUDIO
#include <machine/soundcard.h>
#endif

#ifdef HAVE_SYS_SOUNDCARD_H
#define HAVE_OSS_AUDIO
#include <sys/soundcard.h>
#endif

#ifdef HAVE_SYS_AUDIO_H
#include <sys/audio.h>
#endif

#include <unistd.h>

using namespace std;

class Sound;

DECLARE_NODE(Sound)
/*Node
 *
 * @name Sound
 * @category DSP:Audio
 * @description Opens a sound device
 *
 * @output_name OUTPUT
 * @output_description A file descriptor to the sound device
 *
 * @parameter_name DEVICE
 * @parameter_type string
 * @parameter_value /dev/dsp
 * @parameter_description Path to the sound device
 *
 * @parameter_name RATE
 * @parameter_type int
 * @parameter_description Sampling rate
 *
 * @parameter_name STEREO
 * @parameter_type int
 * @parameter_description 1 for stereo, 0 for mono
 *
 * @parameter_name MODE
 * @parameter_type string
 * @parameter_value W
 * @parameter_description R for sound input, W for sound output, RW for full-duplex mode
 *
 * @parameter_name BUFFER
 * @parameter_type int
 * @parameter_description Length of the audio buffer to allocate (not reliable)
 *
 * @parameter_name DUMMY
 * @parameter_type any
 * @parameter_description Put something here to output to a file
 *
END*/


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
   {
      outputID = addOutput("OUTPUT");
      
   }

   void initialize()
   {
      Node::initialize();

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
      
      if (parameters.exist("DUMMY"))
      {
	 rt_assert((audio_fd=open(device.c_str(),mode, 0644)) != -1, "Can't open sound file\n", __FILE__, __LINE__);
      } else {
	 rt_assert((audio_fd=open(device.c_str(),mode)) != -1, "Can't open sound device\n", __FILE__, __LINE__);
      }

#ifdef HAVE_OSS_AUDIO

      
      if (!parameters.exist("DUMMY"))
      {
	 //int arg=0x7fff0004;
	 int arg=0x7fff000a;
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
	    arg |= 0x7fff0000;
	    //cerr << "arg = " << arg << endl;
	 }      
	 ioctl(audio_fd, SNDCTL_DSP_SETFRAGMENT, &arg);
	 
	 
	 int format=AFMT_S16_LE;
	 if (ioctl(audio_fd, SNDCTL_DSP_SETFMT, &format)==-1)
	 {
	    perror("SNDCTL_DSP_SETFMT");
	    close(audio_fd);
	    throw_error (true, "Can't set the sample format\n", __FILE__, __LINE__);
	 }
	 
	 if (ioctl(audio_fd, SNDCTL_DSP_STEREO, &stereo)==-1)
	 {
	    perror("SNDCTL_DSP_STEREO");
	    close(audio_fd);
	    throw_error (true, "Can't set/reset stereo mode\n", __FILE__, __LINE__);
	 }
	 
	 if (ioctl(audio_fd, SNDCTL_DSP_SPEED, &speed)==-1)
	 {
	    perror("SNDCTL_DSP_SPEED");
	    close(audio_fd);
	    throw_error (true, "Can't set sound device speed\n", __FILE__, __LINE__);
	 }
      }

#endif

      //FIXME: Theoretically, we should choose the type of stream according to "MODE"
      value = ObjectRef(new IOStream(new fd_iostream(audio_fd, true)));
   }

   virtual ~Sound()
   {
      //cerr << "Sound destructor\n";
      //close(audio_fd);
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

