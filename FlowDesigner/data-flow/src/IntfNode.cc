// Copyright (C) 2001 Jean-Marc Valin

#include "IntfNode.h"

//@implements core
using namespace std;

namespace FD {

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
}

void IntfNode::init()
{
   buff = new Buffer(lookAhead+lookBack+1);
   //cerr << "buffsize: " << lookAhead+lookBack+1 << endl;
   //for (int i=0;i<lookAhead;i++)
   //   (*buff)[i] = Object::before_beginningObject;
}

void IntfNode::initialize()
{
   init();
   //cerr << "lookahead: " << lookAhead << endl;
   //cerr << "lookback: " << lookBack << endl;
   Node::initialize();
}

void IntfNode::reset()
{
   init();
   Node::reset();
}

ObjectRef IntfNode::getOutput(int output_id, int count)
{
   //cerr << "get " << count << " " << buff->getCurrentPos() << endl;
   return (*buff).get(count);
   //return (*buff)[count];
}

void IntfNode::setValue(int count, ObjectRef val)
{
   //cerr << "set " << count << " " << buff->getCurrentPos() << endl;
   //(*buff)[count+lookAhead] = val;
   (*buff)[count] = val;
}

}//namespace FD
