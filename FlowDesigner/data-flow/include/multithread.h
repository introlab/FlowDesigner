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

#ifndef MULTITHREAD_H
#define MULTITHREAD_H

#ifdef MULTITHREAD

#include "Node.h"
#include <set>
#include <pthread.h>

class ThreadSet {
   set<pthread_t> runningThreads;
public:
   void start(void * (*funct) (void *), void *args)
   {
      pthread_t newThread;
      pthread_create(&newThread, NULL, funct, args);
      runningThreads.insert(newThread);
   }
   void wait()
   {
      for (set<pthread_t>::iterator thread = runningThreads.begin() ; thread != runningThreads.end() ; thread++)
      {
         pthread_join(*thread,NULL);
         runningThreads.erase(*thread);
      }
   }
   ~ThreadSet() {wait();}
};



class ThreadedGetOutputArgs {
public:
   ThreadedGetOutputArgs (ObjectRef *_ref, Node *_node, int _outputID, int _count) 
      : ref(_ref) , node(_node) , outputID(_outputID) , count(_count)
   {}
   ObjectRef *ref;
   Node *node;
   int outputID;
   int count;
};


inline void getOutputFromThread (ThreadedGetOutputArgs *args)
{
   //cerr << "Launching node " << args->node->getName() << " for count " << args->count << endl;
   *(args->ref) = args->node->getOutput(args->outputID, args->count);
   //cerr << "Ending node " << args->node->getName() << " for count " << args->count << endl;
   delete args;
}

class NodeThreadSet : public ThreadSet
{
public:
   void getOutput (ObjectRef *ref, Node *node, int outputID, int count)
   {
      ThreadedGetOutputArgs *args = new ThreadedGetOutputArgs(ref, node, outputID, count);

      start ((void * (*)(void *)) getOutputFromThread, args);
   }
};

#endif

#endif
