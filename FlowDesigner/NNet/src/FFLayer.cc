#include "FFLayer.h"
#include <stdlib.h>

FFLayer::FFLayer (int _nbNeurons, int _nbInputs)
   : nbNeurons(_nbNeurons)
   , nbInputs (_nbInputs)
{
   weights = new float [nbNeurons*(nbInputs+1)];
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
   {
      //weights[i]=1.0;
      weights[i]=(rand()%1000) * .002 - .05;
   }

   tmp_weights = new float [nbNeurons*(nbInputs+1)];

   value = new float [nbNeurons];
   error = new float [nbNeurons];
}
