// Copyright (C) 2001 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


#ifndef CWRAPPER_H
#define CWRAPPER_H

#ifdef __cplusplus
extern "C" {
#endif 

   /**Initializes the Overflow library*/
   void overflowInitialize(void);

   /**Loads an Overflow .n document*/
   void *overflowLoadDocument(char *filename);

   /**Runs a document that returns a frame buffer*/
   int overflowProcessAudioFile(void *vdoc, char **argv, int *length, int *nbFeatures, float **data);

   /**Create an Overflow network from a document*/
   void *overflowNewNetwork(void *vdoc, char **argv);

   /**Processes one frame*/
   int overflowProcessFrame(void *vnet, float *in, int inLength, float **out, int *outLength);

   /**Destroys an Overflow network*/
   void destroyNetwork(void *vnet);

   /**Destroys (closes) an Overflow document*/
   void destroyDocument(void *vdoc);

#ifdef __cplusplus
}
#endif

#endif
