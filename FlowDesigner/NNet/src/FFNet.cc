#include "FFNet.h"

#include <vector>

FFNet::FFNet(const vector<int> &_topo)
   : topo(_topo)
   , layers(topo.size()-1)
{
   for (int i=0;i<topo.size()-1;i++)
   {
      layers[i]=new FFLayer(topo[i+1],topo[i]);
   }

   float *f=layers[0]->getWeights(0);
   f[0]=f[1]=1;
   f[2]=-.5;
}
