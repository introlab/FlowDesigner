#include "FFLayer.h"

FFLayer::FFLayer (int _nbNeurons, int _nbInputs)
   : nbNeurons(_nbNeurons)
   , nbInputs (_nbInputs)
{
   weights = new float [nbNeurons*(nbInputs+1)];
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
   {
      weights[i]=0.0;
   }
   value = new float [nbNeurons];
   error = new float [nbNeurons];
}
