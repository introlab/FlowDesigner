// Copyright (C) 2001 Jean-Marc Valin


#include "wrapper.h"
#include "UIDocument.h"
#include "Network.h"
#include "ParameterSet.h"
#include "IntfNode.h"

using namespace std;

OFWrapper::OFWrapper(UIDocument* _doc)
   : doc(_doc)
   , net(NULL)
   , count(0)
   , intf(NULL)
{
}

OFWrapper::~OFWrapper()
{
   if (net)
   {
      net->cleanupNotify();
      delete net;
   }
   if (intf)
      delete intf;
}

void OFWrapper::init(const ParameterSet &params, bool _withInput)
{
   withInput = _withInput;
   if (!doc)
      throw new GeneralException("No Overflow document is opened", __FILE__, __LINE__);
   count=0;
   if (intf)
      delete intf;
   if (net)
      delete net;
   try 
   {
      net = doc->getNetworkNamed("MAIN")->build("wrapper", params);
      if (_withInput)
      {
	 intf=new IntfNode("interface", ParameterSet());
	 net->connectToNode("INPUT", intf, "OUTPUT");
      }
      net->verifyConnect();
      net->initialize();
   } catch (...)
   {
      if (net)
      {
	 net->cleanupNotify();
	 delete net;
      }
      throw;
   }
}

ObjectRef OFWrapper::process(ObjectRef in)
{
   if (!net)
      throw new GeneralException("Overflow wrapper is not initialized", __FILE__, __LINE__);
   if (!withInput)
      throw new GeneralException("You should not specify an input for processing", __FILE__, __LINE__);
   intf->setValue(count, in);
   return net->getOutput(0,count++);
}

ObjectRef OFWrapper::process()
{
   if (!net)
      throw new GeneralException("Overflow wrapper is not initialized", __FILE__, __LINE__);
   if (withInput)
      throw new GeneralException("You should specify an input for processing", __FILE__, __LINE__);
   return net->getOutput(0,count++);
}

