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


#include "cwrapper.h"
#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include "path.h"
#include "Network.h"
#include "Vector.h"
#include "wrapper.h"

/**Initializes the Overflow library*/
void overflowInitialize(void)
{
   scanDL();
   UIDocument::loadAllInfo();
}

void overflowInitializeNoDL(void)
{
   UIDocument::loadAllInfo();
}


/**Loads an Overflow .n document*/
void *overflowLoadDocument(char *filename)
{
   UIDocument *doc = new UIDocument(filename);
   doc->load();
   return (void *)doc;
}


/**Runs a document that returns a frame buffer*/
int overflowProcessAudioFile(void *vdoc, char **argv, int *length, int *nbFeatures, float **data)
{
   ParameterSet param;
   int arg=0;
   while (*argv!=NULL)
   {
      char arg_name[100];
      sprintf (arg_name, "ARG%d", ++arg);
      param.add(arg_name, ObjectRef (new String (*argv)));
      argv++;
   }
   UIDocument *doc = ((UIDocument*)vdoc);
   try {
      //cerr << "building net...\n";
      Network *net = doc->build("MAIN", param);
      if (net->getInputNode())
	 throw new GeneralException ("main network has input node", __FILE__, __LINE__);
      //cerr << "initializing...\n";
      net->initialize();
      //cerr << "running (UIDocument)...\n";
      for (int i = 0; ;i++) 
      {
	 if (!net->hasOutput(i)) 
	    break;
	 ObjectRef result = net->getOutput(i,0);
	 Vector<ObjectRef>  &buff = object_cast<Vector<ObjectRef> > (result);
	 *length = buff.size();
	 *nbFeatures = object_cast<Vector<float> > (buff[0]).size();
	 *data = (float*)malloc(*length**nbFeatures*sizeof(float));
	 for (int i=0;i<*length;i++)
	 {
	    Vector<float> &curr = object_cast<Vector<float> > (buff[i]);
	    if (curr.size() != *nbFeatures)
	       throw GeneralException ("Locus front-end: different vector size in output buffer", __FILE__, __LINE__);
	    for (int j=0;j<*nbFeatures;j++)
	       (*data)[i**nbFeatures+j] = curr[j];
	 }
      }
   } 
   catch (BaseException &e) {
      e.print();
      return 0;
   }
   catch (BaseException *e) {
      e->print();
      return 0;
   }
   catch (...)
   {
      cerr << "unknown exception caught" << endl;
      return 0;
   }

    return 1;

}


/**Create an Overflow network from a document*/
void *overflowNewNetwork(void *vdoc, char **argv)
{
   ParameterSet param;
   int arg=0;
   if (argv)
   while (*argv!=NULL)
   {
      char arg_name[100];
      sprintf (arg_name, "ARG%d", ++arg);
      param.add(arg_name, ObjectRef (new String (*argv)));
      argv++;
   }
   UIDocument *doc = ((UIDocument*)vdoc);
   try {
      OFWrapper *wrap = new OFWrapper(doc);
      wrap->init(param);
      void *ret = (void*)(wrap);
      return ret;
   } catch (BaseException *e) {
      e->print();
      return NULL;
   } catch (...)
   {
      cerr << "unknown exception caught" << endl;
      return NULL;
   }
}


/**Processes one frame*/
int overflowProcessFrame(void *vnet, float *in, int inLength, float **out, int *outLength)
{
   OFWrapper *net = (OFWrapper*)vnet;
   try {
      Vector<float> *v = new Vector<float> (inLength);
      for (int i=0;i<inLength;i++)
	 (*v)[i]=in[i];
      ObjectRef obj(v);
      ObjectRef result = net->process(obj);
      
      Vector<float> &res = object_cast<Vector<float> > (result);
      *outLength = res.size();
      cerr << "size = " << res.size() << endl;
      (*out)=(float *) malloc(sizeof(float)* (*outLength));
      for (int i=0;i<*outLength;i++)
	 (*out)[i] = res[i];
   } catch (BaseException *e) {
      e->print();
      return 0;
   } catch (...)
   {
      cerr << "unknown exception caught" << endl;
      return 0;
   }
   return 1;
}

/**Processes one frame*/
int overflowProcessFrame2(void *vnet, float *in, int inLength, float *out, int outLength)
{
   OFWrapper *net = (OFWrapper*)vnet;
   try {
      Vector<float> *v = new Vector<float> (inLength);
      for (int i=0;i<inLength;i++)
	 (*v)[i]=in[i];
      ObjectRef obj(v);
      ObjectRef result = net->process(obj);
      
      Vector<float> &res = object_cast<Vector<float> > (result);
      if (outLength>res.size())
	 outLength = res.size();
      //cerr << "size = " << res.size() << endl;
      //(*out)=(float *) malloc(sizeof(float)* (*outLength));
      for (int i=0;i<outLength;i++)
	 out[i] = res[i];
   } catch (BaseException *e) {
      e->print();
      return 0;
   } catch (...)
   {
      cerr << "unknown exception caught" << endl;
      return 0;
   }
   return 1;
}

/**Destroys an Overflow network*/
void destroyNetwork(void *vnet)
{
   delete (OFWrapper *)vnet;
}


/**Destroys (closes) an Overflow document*/
void destroyDocument(void *vdoc)
{
   delete (UIDocument *)vdoc;
}
