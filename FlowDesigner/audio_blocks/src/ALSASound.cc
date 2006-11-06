// Copyright (C) 1999-2006 Jean-Marc Valin & Dominic Letourneau

#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>

#include <stdio.h>

#include "stream_wrap.h"
#include "Stream.h"
#include <alsa/asoundlib.h>

#include <sched.h>
#include <sys/mman.h>
#include <bits/mman.h>

namespace FD {

class ALSASound;

DECLARE_NODE(ALSASound)
/*Node
 *
 * @name ALSASound
 * @category DSP:Audio
 * @description Opens a sound device
 *
 * @output_name OUTPUT
 * @output_description A file descriptor to the sound device
 *
 * @parameter_name DEVICE
 * @parameter_type string
 * @parameter_value plughw:0,0
 * @parameter_description ALSA sound device use
 *
 * @parameter_name RATE
 * @parameter_type int
 * @parameter_value 48000
 * @parameter_description Sampling rate
 *
 * @parameter_name CHANNELS
 * @parameter_type int
 * @parameter_description 1 for stereo, 0 for mono
 *
 * @parameter_name MODE
 * @parameter_type string
 * @parameter_value W
 * @parameter_description R for sound input, W for sound output, RW for full-duplex mode
 *
 * @parameter_name PERIOD
 * @parameter_type int
 * @parameter_value 256
 * @parameter_description Length of the audio periods to allocate (not reliable)
 *
 * @parameter_name NB_PERIODS
 * @parameter_type int
 * @parameter_value 2
 * @parameter_description Number of audio periods to allocate
 *
END*/

class alsa_streambuf : public std::streambuf {
   protected:
      virtual int overflow(int = EOF) {std::cerr << "Don't call alsa_streambuf::overflow()" << std::endl;return 1;}
      virtual std::streamsize xsputn(const char *s, std::streamsize n);

      virtual int uflow()  {std::cerr << "Don't call alsa_streambuf::uflow()" << std::endl;return 1;}
      virtual int underflow()  {std::cerr << "Don't call alsa_streambuf::underflow()" << std::endl;return 1;}
      virtual std::streamsize xsgetn(char *s, std::streamsize n);
      virtual int pbackfail(int c) {std::cerr << "Don't call alsa_streambuf::pbackfail()" << std::endl;return 1;}
   public:
      alsa_streambuf(std::string dev, int _mode, unsigned int _rate, int _channels, int _period);
      virtual ~alsa_streambuf();
   protected:
      std::string device_name;
      unsigned int rate;
      int channels;
      int mode;
      int period;
      snd_pcm_t *capture_handle;
      snd_pcm_t *playback_handle;
      //int readN, writeN;
      //struct pollfd *read_fd, *write_fd;
};

class alsa_ostream : public std::ostream {
   alsa_streambuf _streambuffer;
   public:
      alsa_ostream(std::string dev, int _mode, unsigned int _rate, int _channels, int _period)
   : std::ostream(&_streambuffer)
            , _streambuffer (dev, _mode, _rate, _channels, _period)
            {clear();}
};

class alsa_istream : public std::istream {
   alsa_streambuf _streambuffer;
   public:
      alsa_istream(std::string dev, int _mode, unsigned int _rate, int _channels, int _period)
   : std::istream(&_streambuffer)
            , _streambuffer (dev, _mode, _rate, _channels, _period)
            {clear();}
};

class alsa_iostream : public std::iostream {
   alsa_streambuf _streambuffer;
   public:
      alsa_iostream(std::string dev, int _mode, unsigned int _rate, int _channels, int _period)
   : std::iostream(&_streambuffer)
            , _streambuffer (dev, _mode, _rate, _channels, _period)
            {clear();}
};


alsa_streambuf::alsa_streambuf(std::string dev, int _mode, unsigned int _rate, int _channels, int _period)
   : device_name(dev)
      , mode (_mode)
      , rate (_rate)
      , channels (_channels)
      , period (_period)
{
   int dir;
   int err;
   snd_pcm_hw_params_t *hw_params;
   snd_pcm_sw_params_t *sw_params;
   snd_pcm_uframes_t period_size = period;
   snd_pcm_uframes_t buffer_size = 2*period;
   static snd_output_t *jcd_out;
   
   err = snd_output_stdio_attach(&jcd_out, stdout, 0);
   
   if ((err = snd_pcm_open (&capture_handle, device_name.c_str(), SND_PCM_STREAM_CAPTURE, 0)) < 0) {
      fprintf (stderr, "cannot open capture audio device %s (%s)\n",
               device_name.c_str(),
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_malloc (&hw_params)) < 0) {
      fprintf (stderr, "cannot allocate capture hardware parameter structure (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_any (capture_handle, hw_params)) < 0) {
      fprintf (stderr, "cannot initialize capture hardware parameter structure (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_set_access (capture_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
      fprintf (stderr, "cannot set capture access type (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_set_format (capture_handle, hw_params, SND_PCM_FORMAT_S16_LE)) < 0) {
      fprintf (stderr, "cannot set capture sample format (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_set_rate_near (capture_handle, hw_params, &rate, 0)) < 0) {
      fprintf (stderr, "cannot set capture sample rate (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   /*fprintf (stderr, "rate = %d\n", rate);*/

   if ((err = snd_pcm_hw_params_set_channels (capture_handle, hw_params, channels)) < 0) {
      fprintf (stderr, "cannot set capture channel count (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   
   period_size = period;
   dir = 0;
   if ((err = snd_pcm_hw_params_set_period_size_near (capture_handle, hw_params, &period_size, &dir)) < 0) {
      fprintf (stderr, "cannot set capture period size (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   dir = 0;
   if ((err = snd_pcm_hw_params_set_periods (capture_handle, hw_params, 2, 0)) < 0) {
      fprintf (stderr, "cannot set capture number of periods (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   
   buffer_size = period_size * 2;
   dir=0;
   if ((err = snd_pcm_hw_params_set_buffer_size_near (capture_handle, hw_params, &buffer_size)) < 0) {
      fprintf (stderr, "cannot set capture buffer size (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   
   if ((err = snd_pcm_hw_params (capture_handle, hw_params)) < 0) {
      fprintf (stderr, "cannot set capture parameters (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   /*snd_pcm_dump_setup(dev->capture_handle, jcd_out);*/
   snd_pcm_hw_params_free (hw_params);

   if ((err = snd_pcm_sw_params_malloc (&sw_params)) < 0) {
      fprintf (stderr, "cannot allocate capture software parameters structure (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   if ((err = snd_pcm_sw_params_current (capture_handle, sw_params)) < 0) {
      fprintf (stderr, "cannot initialize capture software parameters structure (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   if ((err = snd_pcm_sw_params_set_avail_min (capture_handle, sw_params, period)) < 0) {
      fprintf (stderr, "cannot set capture minimum available count (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }
   if ((err = snd_pcm_sw_params (capture_handle, sw_params)) < 0) {
      fprintf (stderr, "cannot set capture software parameters (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }

   
   if ((err = snd_pcm_prepare (capture_handle)) < 0) {
      fprintf (stderr, "cannot prepare capture audio interface for use (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup capture", __FILE__, __LINE__);
   }

   
   
   
   
   if ((err = snd_pcm_open (&playback_handle, device_name.c_str(), SND_PCM_STREAM_PLAYBACK, 0)) < 0) {
      fprintf (stderr, "cannot open playback audio device %s (%s)\n",
               device_name.c_str(),
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   
   if ((err = snd_pcm_hw_params_malloc (&hw_params)) < 0) {
      fprintf (stderr, "cannot allocate playback hardware parameter structure (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_any (playback_handle, hw_params)) < 0) {
      fprintf (stderr, "cannot initialize playback hardware parameter structure (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_set_access (playback_handle, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0) {
      fprintf (stderr, "cannot set playback access type (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_set_format (playback_handle, hw_params, SND_PCM_FORMAT_S16_LE)) < 0) {
      fprintf (stderr, "cannot set playback sample format (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }

   if ((err = snd_pcm_hw_params_set_rate_near (playback_handle, hw_params, &rate, 0)) < 0) {
      fprintf (stderr, "cannot set playback sample rate (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   /*fprintf (stderr, "rate = %d\n", rate);*/

   if ((err = snd_pcm_hw_params_set_channels (playback_handle, hw_params, channels)) < 0) {
      fprintf (stderr, "cannot set playback channel count (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   
   period_size = period;
   dir = 0;
   if ((err = snd_pcm_hw_params_set_period_size_near (playback_handle, hw_params, &period_size, &dir)) < 0) {
      fprintf (stderr, "cannot set playback period size (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   buffer_size = period_size * 2;
   dir=0;
   if ((err = snd_pcm_hw_params_set_buffer_size_near (playback_handle, hw_params, &buffer_size)) < 0) {
      fprintf (stderr, "cannot set playback buffer size (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }


   if ((err = snd_pcm_hw_params (playback_handle, hw_params)) < 0) {
      fprintf (stderr, "cannot set playback parameters (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }

   /*snd_pcm_dump_setup(dev->playback_handle, jcd_out);*/
   snd_pcm_hw_params_free (hw_params);

   
   if ((err = snd_pcm_sw_params_malloc (&sw_params)) < 0) {
      fprintf (stderr, "cannot allocate playback software parameters structure (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   if ((err = snd_pcm_sw_params_current (playback_handle, sw_params)) < 0) {
      fprintf (stderr, "cannot initialize playback software parameters structure (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   if ((err = snd_pcm_sw_params_set_avail_min (playback_handle, sw_params, period)) < 0) {
      fprintf (stderr, "cannot set playback minimum available count (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   if ((err = snd_pcm_sw_params_set_start_threshold (playback_handle, sw_params, period)) < 0) {
      fprintf (stderr, "cannot set playback start mode (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   if ((err = snd_pcm_sw_params (playback_handle, sw_params)) < 0) {
      fprintf (stderr, "cannot set playback software parameters (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
  
  
   if ((err = snd_pcm_prepare (playback_handle)) < 0) {
      fprintf (stderr, "cannot prepare playback audio interface for use (%s)\n",
               snd_strerror (err));
      throw new GeneralException("Cannot setup playback", __FILE__, __LINE__);
   }
   
#if 0
   readN = snd_pcm_poll_descriptors_count(capture_handle);
   writeN = snd_pcm_poll_descriptors_count(playback_handle);
   read_fd = malloc(readN*sizeof(*read_fd));
   /*printf ("descriptors: %d %d\n", dev->readN, dev->writeN);*/
   if (snd_pcm_poll_descriptors(capture_handle, read_fd, readN) != readN)
   {
      fprintf (stderr, "cannot obtain capture file descriptors (%s)\n",
               snd_strerror (err));
      assert(0);
   }
   
   write_fd = malloc(writeN*sizeof(*read_fd));
   if (snd_pcm_poll_descriptors(playback_handle, write_fd, writeN) != writeN)
   {
      fprintf (stderr, "cannot obtain playback file descriptors (%s)\n",
               snd_strerror (err));
      assert(0);
   }
#endif

   struct sched_param param;
   param.sched_priority = sched_get_priority_max(SCHED_FIFO);
   //param.sched_priority = 40;
   if (sched_setscheduler(0,SCHED_FIFO,&param))
      perror("sched_setscheduler");

   if (mlockall(MCL_FUTURE)!=0)
      perror("mlockall");

   {
      short zeros[channels*period];
      for (int i=0;i<channels*period;i++)
         zeros[i] = 0;
      xsputn((char*)zeros, 2*channels*period);
      xsputn((char*)zeros, 2*channels*period);
   }
}

alsa_streambuf::~alsa_streambuf()
{
   snd_pcm_close(playback_handle);
   snd_pcm_close(capture_handle);
}

std::streamsize alsa_streambuf::xsputn(const char *s, std::streamsize n)
{
   int err;
   int nsamples = n/2/channels;
   if ((err = snd_pcm_writei (playback_handle, s, nsamples)) != nsamples)
   {
      if (err<0)
      {
         //fprintf(stderr, "error %d, EPIPE = %d\n", err, EPIPE);
         if (err == -EPIPE)
         {
            fprintf (stderr, "An underrun has occured, reseting playback, len=%d\n", nsamples);
         } else
         {
            fprintf (stderr, "write to audio interface failed (%s)\n",
                     snd_strerror (err));
         }
         if ((err = snd_pcm_prepare (playback_handle)) < 0)
         {
            fprintf (stderr, "cannot prepare audio interface for use (%s)\n",
                     snd_strerror (err));
         }
      } else {
         fprintf (stderr, "Couldn't write as many samples as I wanted (%d instead of %d)\n", err, nsamples);
      }
      //return 0;
   }
   //std::cerr << "wrote " << err << std::endl;
   //std::cerr << "+";
   return n;
}

std::streamsize alsa_streambuf::xsgetn(char *s, std::streamsize n)
{
   int err;
   int nsamples = n/2/channels;
   if ((err = snd_pcm_readi (capture_handle, s, nsamples)) != nsamples)
   {
      if (err<0)
      {
         //fprintf(stderr, "error %d, EPIPE = %d\n", err, EPIPE);
         if (err == -EPIPE)
         {
            fprintf (stderr, "An overrun has occured, reseting capture\n");
         } else
         {
            fprintf (stderr, "read from audio interface failed (%s)\n",
                     snd_strerror (err));
         }
         if ((err = snd_pcm_prepare (capture_handle)) < 0)
         {
            fprintf (stderr, "cannot prepare audio interface for use (%s)\n",
                     snd_strerror (err));
         }
         if ((err = snd_pcm_start (capture_handle)) < 0)
         {
            fprintf (stderr, "cannot prepare audio interface for use (%s)\n",
                     snd_strerror (err));
         }
      } else {
         fprintf (stderr, "Couldn't read as many samples as I wanted (%d instead of %d)\n", err, nsamples);
      }
      //return 0;
   }
   return n;
}

/** A constant node contains a value that will never changes. */
class ALSASound : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;
   
   /*the file descriptor*/
   int audio_fd;
   
   /**The ID of the 'value' output*/
   int outputID;
   String device;
   int speed;
   int channels; 
   int period; 
   int nb_periods;
   String mode;
   bool devOpen;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   ALSASound(std::string nodeName, ParameterSet params)
   : Node(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
   }
   
   void initialize()
   {
      Node::initialize();
      
      device = object_cast <String> (parameters.get("DEVICE"));
      speed = dereference_cast<int> (parameters.get("RATE"));
      channels = dereference_cast<int> (parameters.get("CHANNELS")); 
      period = dereference_cast<int> (parameters.get("PERIOD")); 
      nb_periods = dereference_cast<int> (parameters.get("NB_PERIODS"));
      mode = object_cast <String> (parameters.get("MODE"));
      
      devOpen = false;
   }

   virtual ~ALSASound()
   {
      //cerr << "Sound destructor\n";
      //close(audio_fd);
   }
   /**Ask for the node's output which ID (number) is output_id 
   and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (!devOpen) 
      {
         std::cerr << "Opening audio device " << device << std::endl;
         value = ObjectRef(new IOStream(new alsa_iostream(device, 0, speed, channels, period)));
         std::cerr << "done" << std::endl;
         devOpen = true;
      }
      if (output_id==outputID) return value;
      else throw new NodeException (this, "Sound: Unknown output id", __FILE__, __LINE__);
   }
   
protected:
   /**Default constructor, should not be used*/
   ALSASound() {throw new GeneralException("Sound copy constructor should not be called",__FILE__,__LINE__);}
   
};

}//namespace FD
