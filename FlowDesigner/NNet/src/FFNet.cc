#include "FFNet.h"

#include <vector>

FFNet::FFNet(const vector<int> &_topo)
   : topo(_topo)
   , layers(topo.size()-1)
{
   for (int i=0;i<topo.size()-1;i++)
   {
      if (i==topo.size()-2 && 0)
      {
	 //cout << "layer with " << topo[i+1] << endl;
	 layers[i]=new FFLayer(topo[i+1],topo[i], lin, deriv_lin);
      } else
	 layers[i]=new FFLayer(topo[i+1],topo[i]);
   }

   /*float *f=layers[0]->getWeights(0);
   f[0]=2;f[1]=-1;
   f[2]=1;*/
}
