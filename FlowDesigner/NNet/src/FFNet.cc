// Copyright (C) 2001 Jean-Marc Valin

#include "FFNet.h"

#include <vector>
#include "ObjectParser.h"

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#include "misc.h"
#include "Array.h"

DECLARE_TYPE(FFNet)

FFNet::FFNet(const Vector<int> &_topo, const vector<string> &functions)
   : topo(_topo)
   , layers(topo.size()-1)
{
   init(functions);
}


void FFNet::init(const vector<string> &functions)
{
   nbNeurons = 0;
   nbWeights = 0;
   for (int i=0;i<topo.size()-1;i++)
   {
      nbWeights += (topo[i]+1)*topo[i+1];
      nbNeurons += topo[i+1];
   }
   weights = new float [nbWeights];

   int weightOffset = 0;
   int neuronOffset = 0;
   for (int i=0;i<topo.size()-1;i++)
   {
      layers[i]=new FFLayer(topo[i+1], topo[i], weights, weightOffset, neuronOffset, functions[i]);
      //layers[i]->init(1.0);
      weightOffset += (topo[i]+1)*topo[i+1];
      neuronOffset += topo[i+1];
   }
}


FFNet::FFNet(const Vector<int> &_topo, const vector<string> &functions, vector<float *> &tin, vector<float *> &tout)
   : topo(_topo)
   , layers(topo.size()-1)
{
   init(functions);

   //cerr << tin.size() << endl;
   vector<double> inputMeans(topo[0], 0);
   vector<double> outputMeans(topo[topo.size()-1], 0);
   vector<double> inputStd(topo[0], 0);
   vector<double> outputStd(topo[topo.size()-1], 0);
   
   for (int i=0;i<tin.size();i++)
      for (int j=0;j<topo[0];j++)
	 inputMeans[j] += tin[i][j];


   for (int j=0;j<topo[0];j++)
      inputMeans[j] /= tin.size();

   for (int i=0;i<tin.size();i++)
      for (int j=0;j<topo[0];j++)
	 inputStd[j] += sqr(tin[i][j]-inputMeans[j]);


   for (int j=0;j<topo[0];j++)
       inputStd[j] = sqrt(inputStd[j]/tin.size());
   
   for (int i=0;i<tout.size();i++)
      for (int j=0;j<topo[topo.size()-1];j++)
	 outputMeans[j] += tout[i][j];

   for (int j=0;j<topo[topo.size()-1];j++)
      outputMeans[j] /= tout.size();

   for (int i=0;i<tout.size();i++)
      for (int j=0;j<topo[topo.size()-1];j++)
	 outputStd[j] += (tout[i][j]-outputMeans[j]);
   
   //cerr << endl;
   for (int j=0;j<topo[topo.size()-1];j++)
      outputStd[j] = sqrt(outputStd[j]/tout.size());
   
   
   for (int i=0;i<topo.size()-1;i++)
   {
      //layers[i]=new FFLayer(topo[i+1],topo[i], functions[i]);
      if (i==0)
      {
	 layers[i]->init(&inputMeans[0], &inputStd[0]);
	 //layers[i]->init(10);
      } else { 
	 //layers[i]->init(10.0);
	 layers[i]->init(1.0);
      }
      if (i==topo.size()-2)
	 layers[i]->setBias(&outputMeans[0]);
   }
}

FFNet::FFNet(FFNet &net)
   : topo(net.topo)
   , layers(net.layers.size())
{
   cerr << "I wouldn't do that if I were you...\n";
//   for (int i=0;i<layers.size();i++)
//      layers[i] = new FFLayer(*(net.layers[i]));
}

void FFNet::setupLayersAfterRead()
{
   nbNeurons = 0;
   nbWeights = 0;
   for (int i=0;i<topo.size()-1;i++)
   {
      nbWeights += (topo[i]+1)*topo[i+1];
      nbNeurons += topo[i+1];
   }
   weights = new float [nbWeights];
   
   int weightOffset = 0;
   int neuronOffset = 0;
   for (int i=0;i<topo.size()-1;i++)
   {
      //layers[i]=new FFLayer(topo[i+1], topo[i], weights, weightOffset, neuronOffset, functions[i]);
      //layers[i]->init(1.0);
      layers[i]->setupAfterRead(weights, weightOffset, neuronOffset);
      weightOffset += (topo[i]+1)*topo[i+1];
      neuronOffset += topo[i+1];
   }

}

void FFNet::learn(float *input, float *output, double *gradient, double *err, float *calc_output)
{
   int outputLayer = topo.size()-2;

   float value[nbNeurons];
   float deriv[nbNeurons];
   float error[nbNeurons];
   float fgradient[nbWeights];
   float *calc_out = calc(input, value, deriv);

   if (calc_output)
      for (int i=0;i<topo[topo.size()-1];i++)
	 calc_output[i]=calc_out[i];

   if (err)
   {
      *err += vec_dist2(calc_out,output, topo[topo.size()-1]);
      //cout << calc_out[0] << " " << output[0] << " " << vec_dist2(calc_out,output, topo[topo.size()-1]) << endl;
   }
   
   //start with the output layer, towards the input
   for (int k=outputLayer;k>=0;k--)
   {
      FFLayer *currentLayer = layers[k];
      
      float *previousValue, *currentValue;
      if (k==0)
	 previousValue = input;
      else 
	 previousValue = value + layers[k-1]->getNeuronOffset();

      currentValue = value + currentLayer->getNeuronOffset();
      int layerSize = topo[k+1];
      int layerInputs = topo[k];
      float *delta = error + currentLayer->getNeuronOffset();

      if (k==outputLayer)
      {
	 // Error calculation is simple

	 for (int i=0;i<layerSize;i++)
	    delta[i]=output[i]-currentValue[i];
      } else {
	 // Perform error backpropagation

	 for (int i=0;i<layerSize;i++)
	    delta[i] = 0;
	 float *outErrPtr = error + layers[k+1]->getNeuronOffset();
	 for (int j=0;j<topo[k+2];j++)
	 {
	    float *outW = layers[k+1]->getWeights(j);
	    float outErr = outErrPtr[j];
	    vec_mul_and_add(outErr, outW, delta, layerSize);
	 }
      }

      for (int i=0;i<layerSize;i++)
      {
	 float *grad = fgradient + currentLayer->getNeuronWeightOffset(i);
	 delta[i] = deriv[i+currentLayer->getNeuronOffset()]*delta[i];
	 
	 vec_mul_scal (delta[i], previousValue, grad, layerInputs);
	 grad[layerInputs] = delta[i];
      }
      
      
   }

   /* Trying to prefetch the gradient... not much speadup though ;-)
   int chunks = (nbWeights>>5);
   vec_prefetchnta(gradient, 32);
   for (int j=0;j<chunks;j++)
   {
      if (j!=chunks-1)
	 vec_prefetchnta(gradient+32*(j+1), 32);
      for (int i=32*j;i<32*(j+1);i++)
	 gradient[i] += fgradient[i];
   }
   for (int i=32*chunks;i<nbWeights;i++)
      gradient[i] += fgradient[i];
   */
   for (int i=0;i<nbWeights;i++)
      gradient[i] += fgradient[i];
}


void FFNet::calcGradient(vector<float *> &tin, vector<float *> &tout, Array<float> eval_weights, Array<double> &gradient, double &err)
{
   int i,j;

   float tmp[nbWeights];
   for (int i=0;i<nbWeights;i++)
   {
      tmp[i] = weights[i];
      weights[i] = eval_weights[i];
   }
   //float *tmp = weights;
   //weights = &eval_weights[0];
   
   err=0;
   for (i=0;i<nbWeights;i++)
      gradient[i] = 0;

   for (i=0;i<tin.size();i++)
   {
      if (i != tin.size()-1)
      {
	 vec_prefetchnta(tin[i+1], topo[0]);
	 vec_prefetchnta(tout[i+1], topo[topo.size()-1]);
      }
      learn (tin[i], tout[i], &gradient[0], &err);
   }

   //getGradient(&gradient[0]);
   gradient = -gradient;
   //cerr << gradient[0] << endl;
   //vec_prod_scalar(gradient, -1.0, gradient, );

   for (int i=0;i<nbWeights;i++)
      weights[i] = tmp[i];

   //weights = tmp;
}


void FFNet::weightedLearn(float *input, float *output, float *learnWeights, double *gradient, double *err, float *calc_output)
{
   int outputLayer = topo.size()-2;

   float value[nbNeurons];
   float deriv[nbNeurons];
   float error[nbNeurons];
   float fgradient[nbWeights];
   float *calc_out = calc(input, value, deriv);

   if (calc_output)
      for (int i=0;i<topo[topo.size()-1];i++)
	 calc_output[i]=calc_out[i];

   if (err)
   {
      for (int i=0;i<topo[topo.size()-1];i++)
	 *err += sqr(calc_out[i]-output[i])*learnWeights[i];
      //*err += vec_dist2(calc_out,output, topo[topo.size()-1]);
      //cout << calc_out[0] << " " << output[0] << " " << vec_dist2(calc_out,output, topo[topo.size()-1]) << endl;
   }
   
   //start with the output layer, towards the input
   for (int k=outputLayer;k>=0;k--)
   {
      FFLayer *currentLayer = layers[k];
      
      float *previousValue, *currentValue;
      if (k==0)
	 previousValue = input;
      else 
	 previousValue = value + layers[k-1]->getNeuronOffset();

      currentValue = value + currentLayer->getNeuronOffset();
      int layerSize = topo[k+1];
      int layerInputs = topo[k];
      float *delta = error + currentLayer->getNeuronOffset();

      if (k==outputLayer)
      {
	 // Error calculation is simple

	 for (int i=0;i<layerSize;i++)
	    delta[i]=(output[i]-currentValue[i])*learnWeights[i];
      } else {
	 // Perform error backpropagation

	 for (int i=0;i<layerSize;i++)
	    delta[i] = 0;
	 float *outErrPtr = error + layers[k+1]->getNeuronOffset();
	 for (int j=0;j<topo[k+2];j++)
	 {
	    float *outW = layers[k+1]->getWeights(j);
	    float outErr = outErrPtr[j];
	    vec_mul_and_add(outErr, outW, delta, layerSize);
	 }
      }

      for (int i=0;i<layerSize;i++)
      {
	 float *grad = fgradient + currentLayer->getNeuronWeightOffset(i);
	 delta[i] = deriv[i+currentLayer->getNeuronOffset()]*delta[i];
	 
	 vec_mul_scal (delta[i], previousValue, grad, layerInputs);
	 grad[layerInputs] = delta[i];
      }
      
      
   }

   /* Trying to prefetch the gradient... not much speadup though ;-)
   int chunks = (nbWeights>>5);
   vec_prefetchnta(gradient, 32);
   for (int j=0;j<chunks;j++)
   {
      if (j!=chunks-1)
	 vec_prefetchnta(gradient+32*(j+1), 32);
      for (int i=32*j;i<32*(j+1);i++)
	 gradient[i] += fgradient[i];
   }
   for (int i=32*chunks;i<nbWeights;i++)
      gradient[i] += fgradient[i];
   */
   for (int i=0;i<nbWeights;i++)
      gradient[i] += fgradient[i];
}


void FFNet::weightedCalcGradient(vector<float *> &tin, vector<float *> &tout, vector<float *> &learnWeights, 
				 Array<float> eval_weights, Array<double> &gradient, double &err)
{
   int i,j;

   float tmp[nbWeights];
   for (int i=0;i<nbWeights;i++)
   {
      tmp[i] = weights[i];
      weights[i] = eval_weights[i];
   }
   //float *tmp = weights;
   //weights = &eval_weights[0];
   
   err=0;
   for (i=0;i<nbWeights;i++)
      gradient[i] = 0;

   for (i=0;i<tin.size();i++)
   {
      if (i != tin.size()-1)
      {
	 vec_prefetchnta(tin[i+1], topo[0]);
	 vec_prefetchnta(tout[i+1], topo[topo.size()-1]);
      }
      weightedLearn (tin[i], tout[i], learnWeights[i], &gradient[0], &err);
   }

   //getGradient(&gradient[0]);
   gradient = -gradient;
   //cerr << gradient[0] << endl;
   //vec_prod_scalar(gradient, -1.0, gradient, );

   for (int i=0;i<nbWeights;i++)
      weights[i] = tmp[i];

   //weights = tmp;
}


float FFNet::totalError(vector<float *> tin, vector<float *> tout)
{

   double SSE=0;

   Array<float> wk(nbWeights);
   vec_copy(weights, &wk[0], nbWeights);
   //getWeights(&wk[0]);
   Array<double> dEk(nbWeights);
   calcGradient(tin, tout, wk, dEk, SSE);
   return SSE;
}





void FFNet::setDerivOffset(float d)
{
   for (int i=0;i<layers.size();i++)
      layers[i]->setDerivOffset(d);
}

void FFNet::printOn(ostream &out) const
{
   out << "<FFNet " << endl;
   out << "<topo " << topo << ">" << endl;
   out << "<layers " << layers << ">" << endl;

   out << ">\n";
}


void FFNet::readFrom (istream &in)
{
   string tag;
   //cerr << "FFNet::readFrom\n";
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("Parse error: '<' expected");
      in >> tag;
      if (tag == "topo")
         in >> topo;
      else if (tag == "layers")
      {
         in >> layers;
      }
      else
         throw new ParsingException ("unknown argument: " + tag);
      if (!in) throw new ParsingException ("Parse error trying to build " + tag);
      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
   setupLayersAfterRead();
}

istream &operator >> (istream &in, FFNet &net)
{
   if (!isValidType(in, "FFNet")) return in;
   net.readFrom(in);
   return in;
}
