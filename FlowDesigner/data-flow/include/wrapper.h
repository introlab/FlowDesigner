// Copyright (C) 2001 Jean-Marc Valin

#ifndef WRAPPER_H
#define WRAPPER_H

using namespace std;

#include "UIDocument.h"
#include "Network.h"
#include "ParameterSet.h"
#include "IntfNode.h"

class OFWrapper {
   UIDocument *doc;
   Network *net;
   int count;
   IntfNode *intf;
public:
   OFWrapper(UIDocument *_doc);
   ~OFWrapper();
   void init(const ParameterSet &params);
   ObjectRef process(ObjectRef in);
};




#endif
