// Copyright (C) 2001 Jean-Marc Valin

#ifndef INTFNODE_H
#define INTFNODE_H

#include "Node.h"
#include "Buffer.h"

namespace FD {

class IntfNode : public Node {
   int outputID;
   RCPtr<Buffer> buff;
   int lookAhead;
   int lookBack;
public:
   IntfNode(std::string nodeName, const ParameterSet &params);
   void initialize();
   void reset();
   ObjectRef getOutput(int output_id, int count);

   void request(int outputID, const ParameterSet &req);
   void init();
   void setValue(int count, ObjectRef val);
   
   int getLookAhead() {return lookAhead;}
   int getLookBack() {return lookBack;}
};

}//namespace FD

#endif
