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

OFWrapper::OFWrapper()
   : doc(NULL)
   , net(NULL)
   , count(0)
   , intf(NULL)
{
}

OFWrapper::OFWrapper(string name)
   : doc(NULL)
   , net(NULL)
   , count(0)
   , intf(NULL)
{
   open(name);
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
   if (doc)
      delete doc;
}

void OFWrapper::open(string name)
{
   if (net)
   {
      net->cleanupNotify();
      delete net;
   }
   if (intf)
      delete intf;
   if (doc)
      delete doc;
   doc = new UIDocument(name);
   doc->load();
}

void OFWrapper::init(const ParameterSet &params)
{
   net = doc->build("wrapper", params);
   intf=new IntfNode("interface", ParameterSet());
   net->connectToNode("INPUT", intf, "OUTPUT");
   net->initialize();
}

ObjectRef OFWrapper::process(ObjectRef in)
{
   intf->setValue(count, in);
   return net->getOutput(0,count++);
}

