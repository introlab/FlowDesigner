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


#include "wrapper.h"
#include "UIDocument.h"
#include "Network.h"
#include "ParameterSet.h"
#include "IntfNode.h"

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

void OFWrapper::init(const ParameterSet &params)
{
   //net = doc->build("wrapper", params);
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
      intf=new IntfNode("interface", ParameterSet());
      net->connectToNode("INPUT", intf, "OUTPUT");
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
   intf->setValue(count, in);
   return net->getOutput(0,count++);
}
