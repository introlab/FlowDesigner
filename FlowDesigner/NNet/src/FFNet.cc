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
   /*nbNeurons = 0;
   nbWeights = 0;
   for (int i=0;i<topo.size()-1;i++)
   {
      nbWeights += (topo[i]+1)*topo[i+1];
      nbNeurons += topo[i+1];
   }
   weights = new float [nbWeights];

   float weightOffset = 0;
   float neuronOffset = 0;
   for (int i=0;i<topo.size()-1;i++)
   {
      layers[i]=new FFLayer(topo[i+1], topo[i], weights, weightOffset, neuronOffset, functions[i]);
      layers[i]->init(1.0);
      weightOffset += (topo[i]+1)*topo[i+1];
      neuronOffset += topo[i+1];
      }*/
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

/*
FFNet::FFNet(const Vector<int> &_topo)
   : topo(_topo)
   , layers(topo.size()-1)
{
   for (int i=0;i<topo.size()-1;i++)
   {
      if (i==topo.size()-2)
      {
	 layers[i]=new FFLayer(topo[i+1],topo[i], "lin");
      } else
	 layers[i]=new FFLayer(topo[i+1],topo[i]);
      layers[i]->init(1.0);
   }
   layers[0]->init(5);
}

*/

FFNet::FFNet(const Vector<int> &_topo, const vector<string> &functions, vector<float *> &tin, vector<float *> &tout)
   : topo(_topo)
   , layers(topo.size()-1)
{
   init(functions);

   //cerr << tin.size() << endl;
   vector<float> inputMeans(topo[0], 0);
   vector<float> outputMeans(topo[topo.size()-1], 0);
   vector<float> inputStd(topo[0], 0);
   vector<float> outputStd(topo[topo.size()-1], 0);
   
   for (int i=0;i<tin.size();i++)
   {
      for (int j=0;j<topo[0];j++)
      {
	 inputMeans[j] += tin[i][j];
	 inputStd[j] += tin[i][j]*tin[i][j];
      }
   }
   for (int j=0;j<topo[0];j++)
      inputMeans[j] /= tin.size();
   for (int j=0;j<topo[0];j++)
       inputStd[j] = sqrt(inputStd[j]/tin.size() - inputMeans[j]*inputMeans[j]);
   
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


/*void FFNet::trainCGB(vector<float *> tin, vector<float *> tout, int iter, float sigma, float lambda)
{
   int i,j;
   //float *in = new float [topo[0]];
   //float *out = new float [topo[topo.size()-1]];
   float SSE;
   int k=1;
   //float sigma = .03;
   float lambda_init = lambda;
   float lambdaBar = 0;
   float sigmak;
   bool success = true;

   int nbWeights = 0;
   for (i=0;i<layers.size();i++)
   {
      nbWeights += layers[i]->getNbWeights();
   }

   cerr << "found " << nbWeights << " weights\n";

   Array<float> pk(nbWeights);
   Array<float> rk(nbWeights);
   Array<float> sk(nbWeights);
   Array<float> wk(nbWeights);
   Array<float> dEk(nbWeights);
   Array<float> dEp(nbWeights);
   Array<float> nextdE(nbWeights);
   float nextE;
   float deltak;

   getWeights(&wk[0]);

   calcGradient(tin, tout, wk, dEk, SSE);
   pk=-dEk;
   rk=-dEk;
   while (k < iter)
   {

      float norm2 = pk.norm2();
      float norm = sqrt(norm2);

      //2. If success
      if (success)
      {
	 sigmak = sigma / norm;
	 float dummy = 0;
	 calcGradient(tin, tout, wk+pk*sigmak, dEp, dummy);
	 sk = (dEp - dEk)*(1/sigmak);
	 deltak = pk*sk;
      }
      
      //3. Scale
      sk += pk*(lambda - lambdaBar);
      deltak += (lambda - lambdaBar) * norm2;
      
      //4. Hessian
      if (deltak <= 0)
      {
	 cerr << "Hessian not positive definite\n";
	 sk += pk*(lambda - 2*deltak/norm2);
	 lambdaBar = 2*(lambda - deltak/norm2);
	 deltak = -deltak + lambda*norm2;
	 lambda = lambdaBar;
      }

      //5. Step size
      float uk = pk * rk;
      float ak = uk/deltak;

      //ak = .000000001;
      //pk = -rk;

      //6. Comparison
      calcGradient(tin, tout, wk+pk*ak, nextdE, nextE);
      float DK = 2*deltak*(SSE -  nextE) / (uk*uk);

      //cerr << SSE << " " << nextE << " " << ak << " " << uk << " " << norm << endl;

      //7. Can we reduce the error
      if (DK >= 0)
      {
	 wk += pk*ak;
	 Array<float> oldR = rk;
	 SSE=nextE;
	 rk = -nextdE;
	 lambdaBar = 0;
	 success = true;
	 cout << SSE/tin.size()/topo[topo.size()-1] << "\t" << DK << "\t" << lambda << "\t" << norm << "\t" << ak << "\t" << endl;
	 if (k%nbWeights == 0)
	 {
	    pk = rk;
	    k++;
	    lambda = lambda_init;
	    lambdaBar = 0;
	    cerr << "restarting\n";
	    continue;
	 } else {
	    float bk = (rk.norm2() - rk*oldR)/uk;
	    pk = rk + pk * bk;
	 }
	 if (DK >= .75 && lambda > 1e-100)
	    lambda *= .5;
	 k++;
      } else {
	 lambdaBar = lambda;
	 success = false;
      }

      //8. increase scale
      if (DK < .25 && lambda < 1e200)
	 lambda *= 4;
      
      //9. Have we found the minimum
      if (rk.norm() == 0)
	 break;
      //k++;
   }
   setWeights(&wk[0]);
}
*/

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


void FFNet::trainDeltaBar(vector<float *> tin, vector<float *> tout, int iter, float learnRate, 
			  float increase, float decrease)
{
   int i,j;
   double SSE;
   int k=1;

   int nbWeights = 0;
   for (i=0;i<layers.size();i++)
   {
      nbWeights += layers[i]->getNbWeights();
   }

   cerr << "found " << nbWeights << " weights\n";

   Array<float> alpha(nbWeights);
   Array<float> wk(nbWeights);
   Array<float> nextW(nbWeights);
   Array<double> dEk(nbWeights);
   Array<double> nextdE(nbWeights);
   double nextE;

   vec_copy(weights, &wk[0], nbWeights);

   for (i=0;i<nbWeights;i++)
      alpha[i] = learnRate;

   calcGradient(tin, tout, wk, dEk, SSE);
   while (iter--)
   {

      float norm = dEk.norm();
      float norm_1 = 1;// / norm;
      
      for (i=0;i<nbWeights;i++)
	 nextW[i] = wk[i] - alpha[i] * norm_1 * dEk[i];
      calcGradient(tin, tout, nextW, nextdE, nextE);
      
      if (nextE > SSE)
      {
	 learnRate *= decrease;
	 //So that the "bad" iteration doesn't count
	 iter++;
	 cerr << "backing off\n";
	 continue;
      }

      for (i=0;i<nbWeights;i++)
      {
	 if (nextdE[i]*dEk[i] >= 0)
	    alpha[i] *= increase;
	 else
	    alpha[i] *= decrease;
	 if (alpha[i] < 1e-58)
	    alpha[i] = 1e-58;
      }

      //if (SSE/tin.size()/topo[topo.size()-1]<.08) break;
      cout << (SSE/tin.size()/topo[topo.size()-1]) << "\t" << tin.size() << endl;
      SSE=nextE;
      dEk = nextdE;
      wk = nextW;

   }

   vec_copy(&wk[0], weights, nbWeights);
}

void FFNet::trainQProp(vector<float *> tin, vector<float *> tout, int iter, float learnRate)
{

   int i,j;
   double SSE;
   int k=1;

   float increase=1.04;
   float decrease = .6;

   int nbWeights = 0;
   for (i=0;i<layers.size();i++)
   {
      nbWeights += layers[i]->getNbWeights();
   }
   cerr << "WARNING: This implementation of Quickprop doesn't work ywt!" << endl;
   cerr << "found " << nbWeights << " weights\n";

   //Array<float> alpha(nbWeights);
   Array<float> wk(nbWeights);
   Array<float> nextW(nbWeights);

   Array<double> dW(nbWeights,0);
   // Array<double> dWq(nbWeights,0);
   Array<double> prevdW(nbWeights,0);
   Array<double> dE(nbWeights,0);
   Array<double> nextdE(nbWeights,0);
   Array<double> prevdE(nbWeights,0);

   //Array<double> nextdE(nbWeights);
   double nextE;

   vec_copy(weights, &wk[0], nbWeights);

   //for (i=0;i<nbWeights;i++)
   //   alpha[i] = learnRate;

   calcGradient(tin, tout, wk, dE, SSE);
   while (iter--)
   {

      for (i=0;i<nbWeights;i++)
      {
	 double norm = dE[i]-prevdE[i];
	 if (fabs(norm) > 1e-8)
	    dW[i] = -(dE[i]*prevdW[i])/norm;
	 else
	    dW[i] = 0;

	 if ((dE[i]*prevdE[i]) <= 0)
	    dW[i]-= learnRate*dE[i];
      }
      

      for (i=0;i<nbWeights;i++)
	 nextW[i] = wk[i] + dW[i];

      calcGradient(tin, tout, nextW, nextdE, nextE);

      while(nextE > SSE)
      {
	 float alpha = learnRate;
	 for (i=0;i<nbWeights;i++)
	    nextW[i] = wk[i] - alpha*dE[i];
	 calcGradient(tin, tout, nextW, nextdE, nextE);
	 alpha *= .5;
      }


      //if (SSE/tin.size()/topo[topo.size()-1]<.08) break;
      cout << (SSE/tin.size()/topo[topo.size()-1]) << "\t" << tin.size() << endl;
      prevdW=dW;
      prevdE=dE;
      SSE=nextE;
      dE = nextdE;
      wk = nextW;
      //nextdE = dE;
   }

   vec_copy(&wk[0], weights, nbWeights);


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
