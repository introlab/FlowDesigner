// Copyright (C) 2001 Jean-Marc Valin

#ifndef WRAPPER_H
#define WRAPPER_H


#include "UIDocument.h"
#include "Network.h"
#include "ParameterSet.h"
#include "IntfNode.h"

namespace FD {

class OFWrapper {
   UIDocument *doc;
   Network *net;
   int count;
   IntfNode *intf;
   bool withInput;
public:
   OFWrapper(UIDocument *_doc);
   ~OFWrapper();
   void init(const ParameterSet &params, bool withInput=false);
   ObjectRef process(ObjectRef in);
   ObjectRef process();
};

}//end namespace FD


#endif
