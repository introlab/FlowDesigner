// Copyright (C) 2001 Jean-Marc Valin

#include "IntfNode.h"

IntfNode::IntfNode(string nodeName, const ParameterSet &params)
   : Node(nodeName, params)
   , lookAhead(0)
   , lookBack(0)
{
   outputID = addOutput("OUTPUT");
}

void IntfNode::request(int outputID, const ParameterSet &req)
{ 
   if (req.exist("LOOKAHEAD"))
      lookAhead = max(lookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
   if (req.exist("LOOKBACK"))
      lookBack = max(lookBack,dereference_cast<int> (req.get("LOOKBACK")));
   this->Node::request(outputID,req);
}

void IntfNode::init()
{
   buff = new Buffer(lookAhead+lookBack+1);
   //cerr << "buffsize: " << lookAhead+lookBack+1 << endl;
   for (int i=0;i<lookAhead;i++)
      (*buff)[i] = Object::before_beginningObject;
}

void IntfNode::specificInitialize()
{
   init();
   //cerr << "lookahead: " << lookAhead << endl;
   //cerr << "lookback: " << lookBack << endl;
   Node::specificInitialize();
}

void IntfNode::reset()
{
   init();
   Node::reset();
}

ObjectRef IntfNode::getOutput(int output_id, int count)
{
   return (*buff)[count];
}

void IntfNode::setValue(int count, ObjectRef val)
{
   (*buff)[count+lookAhead] = val;
}
