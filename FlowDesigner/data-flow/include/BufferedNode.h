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

#ifndef BUFFERED_NODE_H
#define BUFFERED_NODE_H

#include "Node.h"
#include "Buffer.h"

class OutputCacheInfo {
public:
   OutputCacheInfo () 
      : lookAhead(0)
      , lookBack(0)
      , cacheAll(false)
   {}

   ObjectRef buffer;
   int lookAhead;
   int lookBack;
   bool cacheAll;
};

class InputCacheInfo {
public:
   InputCacheInfo () 
      : lookAhead(0)
      , lookBack(0)
      , cacheAll(false)
   {}
   int lookAhead;
   int lookBack;
   bool cacheAll;
};

class BufferedNode : public Node {
protected:

   vector<OutputCacheInfo> outputs;

   vector<InputCacheInfo> inputsCache;
 
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   BufferedNode(string nodeName, const ParameterSet &params);
   
   /**Destructor*/
   virtual ~BufferedNode() {}

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count); 

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef calculate(int output_id, int count, Buffer &buf) = 0;

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req);

   //virtual void request(const ParameterSet &req) {throw "error: maudit request";}

   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize();

   /**Resets the node internal values and buffers*/
   virtual void reset();

   /**Adding an output to a node*/
   virtual int addOutput (const string &outputName);

   /**Adding an input to a node*/
   virtual int addInput (const string &inputName);

   /***/
   virtual void initializeBuffers();

   /***/
   virtual void performRequests();

protected:
   /**Default constructor, should not be used*/
   BufferedNode() {throw GeneralException("BufferedNode copy constructor should not be called",__FILE__,__LINE__);}
};

#endif
