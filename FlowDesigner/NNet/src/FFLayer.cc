#include "FFLayer.h"
#include <stdlib.h>

FFLayer::FFLayer (int _nbNeurons, int _nbInputs, double (*_func) (double), double (*_deriv_func) (double))
   : nbNeurons(_nbNeurons)
   , nbInputs (_nbInputs)
   , func(_func)
   , deriv_func (_deriv_func)
{
   weights = new float [nbNeurons*(nbInputs+1)];
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
   {
      //weights[i]=1.0;
      weights[i]=(rand()%1000) * .0002 - .05;
   }

   tmp_weights = new float [nbNeurons*(nbInputs+1)];

   deriv = new float [nbNeurons];
   value = new float [nbNeurons];
   error = new float [nbNeurons];
}
