// Copyright (C) 2001 Jean-Marc Valin


#ifndef CWRAPPER_H
#define CWRAPPER_H

#ifdef __cplusplus
extern "C" {
#endif 

   /**Initializes the Overflow library*/
   void overflowInitialize(void);

   void overflowInitializeNoDL(void);
   
   /**Loads an Overflow .n document*/
   void *overflowLoadDocument(const char *filename);

   /**Runs a document that returns a frame buffer*/
   int overflowProcessAudioFile(void *vdoc, const char **argv, int *length, int *nbFeatures, float **data);

   /**Create an Overflow network from a document*/
   void *overflowNewNetwork(void *vdoc, const char **argv);

   /**Processes one frame*/
   int overflowProcessFrame(void *vnet, float *in, int inLength, float **out, int *outLength);

   /**Processes one frame*/
   int overflowProcessFrame2(void *vnet, float *in, int inLength, float *out, int outLength);

   /**Destroys an Overflow network*/
   void destroyNetwork(void *vnet);

   /**Destroys (closes) an Overflow document*/
   void destroyDocument(void *vdoc);

#ifdef __cplusplus
}
#endif

#endif
