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
   //topo = _topo;
   for (int i=0;i<topo.size()-1;i++)
   {
      layers[i]=new FFLayer(topo[i+1],topo[i], functions[i]);
      layers[i]->init(1.0);
   }
}


FFNet::FFNet(const Vector<int> &_topo)
   : topo(_topo)
   , layers(topo.size()-1)
{
   //topo = _topo;
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

/* Now inlined
double *FFNet::calc(const double *input)
{
   layers[0]->update(input);
   for (int i=1;i<layers.size();i++)
      layers[i]->update(layers[i-1]->getValue());
   return layers[layers.size()-1]->getValue();
}
*/

void FFNet::learn(double *input, double *output, double *err, double *calc_output)
{
   int outputLayer = topo.size()-2;
   double *calc_out = calc(input);

   if (calc_output)
      for (int i=0;i<topo[topo.size()-1];i++)
	 calc_output[i]=calc_out[i];

   //double *calc_out = output;
   if (err)
   {
      for (int i=0;i<topo[topo.size()-1];i++)
	 *err += (calc_out[i]-output[i]) * (calc_out[i]-output[i]);
   }
   //start with the output layer, towards the input
   for (int k=outputLayer;k>=0;k--)
   {
      FFLayer *currentLayer = layers[k];
      double *previousValue, *currentValue;
      if (k==0)
	 previousValue = input;
      else 
	 previousValue = layers[k-1]->getValue();

      currentValue = currentLayer->getValue();

      int layerSize = topo[k+1];
      int layerInputs = topo[k];
      double *delta = currentLayer->getError();
      for (int i=0;i<layerSize;i++)
      {
	 double *w = currentLayer->getGradient(i);
	 //cerr << k << endl;
	 if (k==outputLayer)
	 {
	    //cerr << "output layer\n";
	    delta[i]=output[i]-currentValue[i];
	    //error += delta[i]*delta[i];
	    //cout << "error = " << delta[i] << endl;
	    delta[i] = currentLayer->deriv[i]*delta[i];
	 }
	 else
	 {
	    delta[i] = 0;
	    double *outErr = layers[k+1]->getError();
	    /*
	    for (int j=0;j<topo[k+2];j++)
	    {
	       double *outW = layers[k+1]->getWeights(j);
	       delta[i]+= outErr[j]*outW[i];
	    }
	    delta[i] = currentLayer->deriv[i]*delta[i];
                 */

	    //This section is an optimized version of the commented one
	    double *outW = layers[k+1]->getWeights(0)+i;
	    int incr = layerSize+1;

	    for (int j=0;j<topo[k+2];j++)
	    {
	       delta[i]+= *outErr++ * *outW;
	       outW += incr;
            }
	    delta[i] = currentLayer->deriv[i]*delta[i];
	    
	 }

         /*
	 for (int j=0;j<layerInputs;j++)
	 {
	    w[j] += previousValue[j] * delta[i];
	 }
	 w[layerInputs] += delta[i];
         */
	 //This section is an optimized version of the commented one
         double *p=previousValue;
         double d=delta[i];
         double *end=w+layerInputs;

	 while (w<end-3)
	 {
	    *w++ += *p++ * d;
	    *w++ += *p++ * d;
	    *w++ += *p++ * d;
	    *w++ += *p++ * d;
	 }

	 while (w<end)
	    *w++ += *p++ * d;
	 *w += d;

      }
   }	 
}

void FFNet::learn_bounds(double *input, double *output, double *low_bound, double *err, double *calc_output)
{
   int outputLayer = topo.size()-2;
   double *calc_out = calc(input);

   if (calc_output)
      for (int i=0;i<topo[topo.size()-1];i++)
      {
	 calc_output[i]=calc_out[i];
      }
   //double *calc_out = output;
   if (err)
   {
      for (int i=0;i<topo[topo.size()-1];i++)
      {
	 if (output[i] > low_bound[i])
	    *err += (calc_out[i]-output[i]) * (calc_out[i]-output[i]);
	 else if (calc_out[i] > low_bound[i])
	    *err += (calc_out[i]-low_bound[i]) * (calc_out[i]-low_bound[i]);
      }
   }
   //start with the output layer, towards the input
   for (int k=outputLayer;k>=0;k--)
   {
      FFLayer *currentLayer = layers[k];
      double *previousValue, *currentValue;
      if (k==0)
	 previousValue = input;
      else 
	 previousValue = layers[k-1]->getValue();

      currentValue = currentLayer->getValue();

      int layerSize = topo[k+1];
      int layerInputs = topo[k];
      double *delta = currentLayer->getError();
      for (int i=0;i<layerSize;i++)
      {
	 double *w = currentLayer->getGradient(i);
	 //cerr << k << endl;
	 if (k==outputLayer)
	 {
	    //cerr << "output layer\n";
	    if (output[i] > low_bound[i])
	       delta[i]=output[i]-currentValue[i];
	    else if (currentValue[i] > low_bound[i])
	       delta[i] = low_bound[i] - currentValue[i];
	    else 
	       delta[i] = 0;
	       
	    //error += delta[i]*delta[i];
	    //cout << "error = " << delta[i] << endl;
	    delta[i] = currentLayer->deriv[i]*delta[i];
	 }
	 else
	 {
	    delta[i] = 0;
	    double *outErr = layers[k+1]->getError();
	    /*
	    for (int j=0;j<topo[k+2];j++)
	    {
	       double *outW = layers[k+1]->getWeights(j);
	       delta[i]+= outErr[j]*outW[i];
	    }
	    delta[i] = currentLayer->deriv[i]*delta[i];
                 */

	    //This section is an optimized version of the commented one
	    double *outW = layers[k+1]->getWeights(0)+i;
	    int incr = layerSize+1;

	    for (int j=0;j<topo[k+2];j++)
	    {
	       delta[i]+= *outErr++ * *outW;
	       outW += incr;
            }
	    delta[i] = currentLayer->deriv[i]*delta[i];
	    
	 }

         /*
	 for (int j=0;j<layerInputs;j++)
	 {
	    w[j] += previousValue[j] * delta[i];
	 }
	 w[layerInputs] += delta[i];
         */
	 //This section is an optimized version of the commented one
         double *p=previousValue;
         double d=delta[i];
         double *end=w+layerInputs;

	 while (w<end-3)
	 {
	    *w++ += *p++ * d;
	    *w++ += *p++ * d;
	    *w++ += *p++ * d;
	    *w++ += *p++ * d;
	 }

	 while (w<end)
	    *w++ += *p++ * d;
	 *w += d;

      }
   }	 
}


void FFNet::train(vector<float *> tin, vector<float *> tout, int iter, double learnRate, double mom, 
		  double increase, double decrease, double errRatio, int nbSets)
{
   //int worse=0;
   double error;
   double min_error=FLT_MAX;
   double last_error=FLT_MAX;
   double alpha = learnRate;
   double momentum=mom;
   double errRatio2=errRatio;

   double in[topo[0]];
   double out[topo[topo.size()-1]];

   int i,j;

   min_error=0;
   for (i=0;i<tin.size();i++)
   {
      double in[topo[0]];
      double out[topo[topo.size()-1]];
      for (j=0;j<topo[0];j++)
	 in[j]=tin[i][j];
      for (j=0;j<topo[topo.size()-1];j++)
	 out[j]=tout[i][j];
      
      double *netOut = calc (in);
      for (j=0;j<topo[topo.size()-1];j++)
	 min_error += (netOut[j]-out[j])*(netOut[j]-out[j]);
   }

   last_error=min_error;

   while (iter)
   {

	 
      //error = 0;
      //cerr << "iter...\n";
      //int nbSets = 10;

      for (int batchSet=0; batchSet < nbSets; batchSet++)
      {
	 for (i=0;i<layers.size();i++)
	 {
	    layers[i]->resetGradient();
	 }
	 for (i=batchSet;i<tin.size();i+=nbSets)
	 {
	    //double in[topo[0]];
	    //double out[topo[topo.size()-1]];
	    for (j=0;j<topo[0];j++)
	       in[j]=tin[i][j];
	    for (j=0;j<topo[topo.size()-1];j++)
	       out[j]=tout[i][j];
	    learn (in, out);
	    
	 }
	 for (i=0;i<layers.size();i++)
	 {
	    layers[i]->updateGradient(alpha,momentum);
	 }
      }

      iter--;

      double SSE = 0;
      for (i=0;i<tin.size();i++)
      {
	 //double in[topo[0]];
	 //double out[topo[topo.size()-1]];
	 for (j=0;j<topo[0];j++)
	    in[j]=tin[i][j];
	 for (j=0;j<topo[topo.size()-1];j++)
	    out[j]=tout[i][j];

	 double *netOut = calc (in);
	 for (j=0;j<topo[topo.size()-1];j++)
	    SSE += (netOut[j]-out[j])*(netOut[j]-out[j]);
      }
      //momentum=.85;


      if (SSE < min_error)
      {
	 momentum = mom;
	 alpha *= increase;
	 //if (alpha > .0000015) alpha = .0000015;
	 error = SSE;
	 min_error=error;
	 errRatio2=errRatio;
      } else if (SSE<=last_error) 
      { 
	 error=SSE;
	 
      } else if (SSE/errRatio2 > min_error)
      {
	 errRatio2 *= 1.0001;
	 cerr << SSE-last_error << endl;
	 momentum=0;
	 alpha *= decrease;
	 //if (SSE/1.04 > min_error)
	 for (i=0;i<layers.size();i++)
	    layers[i]->undo();
	 
      } else {
	 
	 alpha *= sqrt(decrease);
	 momentum = 0;
	 error=SSE;
      }


      cout << (error/tin.size()/topo[topo.size()-1]) << "\t" << alpha << "\t" << tin.size() << endl;

      last_error = error;
   }
}

void FFNet::getGradient(double *ptr)
{
   for (int i=0;i<layers.size();i++)
   {
      int layerWeights = layers[i]->getNbWeights();
      vec_copy(layers[i]->getGradient(0), ptr, layerWeights);
      ptr += layerWeights;
   }
}

void FFNet::getWeights(double *ptr)
{
   double *tata = ptr;
   for (int i=0;i<layers.size();i++)
   {
      int layerWeights = layers[i]->getNbWeights();
      vec_copy(layers[i]->getWeights(0), ptr, layerWeights);
      //vec_copy(tata, tata, layerWeights);
      //vec_copy(tata, tata, 12);
      //vec_copy(layers[i]->getWeights(0), tata, layerWeights);
      ptr += layerWeights;
   }
}

void FFNet::setWeights(double *ptr)
{
   for (int i=0;i<layers.size();i++)
   {
      int layerWeights = layers[i]->getNbWeights();
      vec_copy(ptr, layers[i]->getWeights(0), layerWeights);
      ptr += layerWeights;
   }
}

void FFNet::calcGradient(vector<float *> &tin, vector<float *> &tout, Array<double> weights, Array<double> &gradient, double &err)
{
   int i,j;
   double *in = new double [topo[0]];
   double *out = new double [topo[topo.size()-1]];
   for (i=0;i<layers.size();i++)
      layers[i]->saveWeights();

   //if (weights)
      setWeights(weights.begin());
   
   err=0;
   for (i=0;i<layers.size();i++)
      layers[i]->resetGradient();
   for (i=0;i<tin.size();i++)
   {
      for (j=0;j<topo[0];j++)
	 in[j]=tin[i][j];
      for (j=0;j<topo[topo.size()-1];j++)
	 out[j]=tout[i][j];
      learn (in, out, &err);
   }

   getGradient(gradient.begin());
   gradient = -gradient;

   //vec_prod_scalar(gradient, -1.0, gradient, );

   for (i=0;i<layers.size();i++)
      layers[i]->loadWeights();

   delete [] in;
   delete [] out;
}

void FFNet::calcGradientBounds(vector<float *> &tin, vector<float *> &tout, vector<float *> &tbounds, Array<double> weights, Array<double> &gradient, double &err)
{
   int i,j;
   double *in = new double [topo[0]];
   double *out = new double [topo[topo.size()-1]];
   double *bounds = new double [topo[topo.size()-1]];
   for (i=0;i<layers.size();i++)
      layers[i]->saveWeights();

   //if (weights)
      setWeights(weights.begin());
   
   err=0;
   for (i=0;i<layers.size();i++)
      layers[i]->resetGradient();
   for (i=0;i<tin.size();i++)
   {
      for (j=0;j<topo[0];j++)
	 in[j]=tin[i][j];
      for (j=0;j<topo[topo.size()-1];j++)
	 out[j]=tout[i][j];
      for (j=0;j<topo[topo.size()-1];j++)
	 bounds[j]=tbounds[i][j];
      learn_bounds (in, out, bounds, &err);
   }

   getGradient(gradient.begin());
   gradient = -gradient;

   //vec_prod_scalar(gradient, -1.0, gradient, );

   for (i=0;i<layers.size();i++)
      layers[i]->loadWeights();

   delete [] in;
   delete [] out;
   delete [] bounds;
}

void FFNet::trainCGB(vector<float *> tin, vector<float *> tout, int iter, double sigma, double lambda)
{
   int i,j;
   //double *in = new double [topo[0]];
   //double *out = new double [topo[topo.size()-1]];
   double SSE;
   int k=1;
   //double sigma = .03;
   double lambda_init = lambda;
   double lambdaBar = 0;
   double sigmak;
   bool success = true;

   int nbWeights = 0;
   for (i=0;i<layers.size();i++)
   {
      nbWeights += layers[i]->getNbWeights();
   }

   cerr << "found " << nbWeights << " weights\n";

   Array<double> pk(nbWeights);
   Array<double> rk(nbWeights);
   Array<double> sk(nbWeights);
   Array<double> wk(nbWeights);
   Array<double> dEk(nbWeights);
   Array<double> dEp(nbWeights);
   Array<double> nextdE(nbWeights);
   double nextE;
   double deltak;

   getWeights(wk.begin());

   calcGradient(tin, tout, wk, dEk, SSE);
   pk=-dEk;
   rk=-dEk;
   while (k < iter)
   {

      double norm2 = pk.norm2();
      double norm = sqrt(norm2);

      //2. If success
      if (success)
      {
	 sigmak = sigma / norm;
	 double dummy = 0;
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
      double uk = pk * rk;
      double ak = uk/deltak;

      //ak = .000000001;
      //pk = -rk;

      //6. Comparison
      calcGradient(tin, tout, wk+pk*ak, nextdE, nextE);
      double DK = 2*deltak*(SSE -  nextE) / (uk*uk);

      //cerr << SSE << " " << nextE << " " << ak << " " << uk << " " << norm << endl;

      //7. Can we reduce the error
      if (DK >= 0)
      {
	 wk += pk*ak;
	 Array<double> oldR = rk;
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
	    double bk = (rk.norm2() - rk*oldR)/uk;
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
   setWeights(wk.begin());
}

/*Scaled conjugate gradient with bounds */
void FFNet::trainSCGBounds(vector<float *> tin, vector<float *> tout, vector<float *> tbounds, int iter, double sigma, double lambda)
{
   int i,j;
   //double *in = new double [topo[0]];
   //double *out = new double [topo[topo.size()-1]];
   double SSE;
   int k=1;
   //double sigma = .03;
   double lambda_init = lambda;
   double lambdaBar = 0;
   double sigmak;
   bool success = true;

   int nbWeights = 0;
   for (i=0;i<layers.size();i++)
   {
      nbWeights += layers[i]->getNbWeights();
   }

   cerr << "found " << nbWeights << " weights\n";

   Array<double> pk(nbWeights);
   Array<double> rk(nbWeights);
   Array<double> sk(nbWeights);
   Array<double> wk(nbWeights);
   Array<double> dEk(nbWeights);
   Array<double> dEp(nbWeights);
   Array<double> nextdE(nbWeights);
   double nextE;
   double deltak;

   getWeights(wk.begin());

   calcGradientBounds(tin, tout, tbounds, wk, dEk, SSE);
   pk=-dEk;
   rk=-dEk;
   while (k < iter)
   {

      double norm2 = pk.norm2();
      double norm = sqrt(norm2);

      //2. If success
      if (success)
      {
	 sigmak = sigma / norm;
	 double dummy = 0;
	 calcGradientBounds(tin, tout, tbounds, wk+pk*sigmak, dEp, dummy);
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
      double uk = pk * rk;
      double ak = uk/deltak;

      //ak = .000000001;
      //pk = -rk;

      //6. Comparison
      calcGradientBounds(tin, tout, tbounds, wk+pk*ak, nextdE, nextE);
      double DK = 2*deltak*(SSE -  nextE) / (uk*uk);

      //cerr << SSE << " " << nextE << " " << ak << " " << uk << " " << norm << endl;

      //7. Can we reduce the error
      if (DK >= 0)
      {
	 wk += pk*ak;
	 Array<double> oldR = rk;
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
	    double bk = (rk.norm2() - rk*oldR)/uk;
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
   setWeights(wk.begin());
}

double FFNet::calcError(const vector<float *> &tin, const vector<float *> &tout)
{
   int i,j;
   double error=0;
   for (i=0;i<tin.size();i++)
   {
      double in[topo[0]];
      double out[topo[topo.size()-1]];
      for (j=0;j<topo[0];j++)
	 in[j]=tin[i][j];
      for (j=0;j<topo[topo.size()-1];j++)
	 out[j]=tout[i][j];
      
      double *netOut = calc (in);
      for (j=0;j<topo[topo.size()-1];j++)
	 error += (netOut[j]-out[j])*(netOut[j]-out[j]);
   }
   return error;
}

void FFNet::traincg(vector<float *> tin, vector<float *> tout, int iter)
{

   int i,j;


   while (iter)
   {



      for (i=0;i<layers.size();i++)
      {
	 layers[i]->resetGradient();
      }
      for (i=0;i<tin.size();i++)
      {
	 double in[topo[0]];
	 double out[topo[topo.size()-1]];
	 for (j=0;j<topo[0];j++)
	    in[j]=tin[i][j];
	 for (j=0;j<topo[topo.size()-1];j++)
	    out[j]=tout[i][j];
	 learn (in, out);
	 
      }

      double min_error = FLT_MAX;
      double min_alpha = 0;
      for (double alpha = .00000000001; alpha < .0001 ; alpha *= 5)
      {
	 for (i=0;i<layers.size();i++)
	    layers[i]->saveWeights();
	 
	 for (i=0;i<layers.size();i++)
	    layers[i]->updateGradient(alpha,0.0);

	 double sse = calcError(tin, tout);
	 if (sse < min_error)
	 {
	    min_error=sse;
	    min_alpha=alpha;
	 }
	 
	 for (i=0;i<layers.size();i++)
	    layers[i]->loadWeights();
	 

      }

      /*for (i=0;i<layers.size();i++)
	 layers[i]->updateGradient(min_alpha,0.0);
      */

      double A=min_alpha/3;
      double B=min_alpha*3;
      double sseA, sseB;
      double middle;
      for (j=0;j<10;j++)
      {
	 for (i=0;i<layers.size();i++)
	    layers[i]->saveWeights();
	 for (i=0;i<layers.size();i++)
	    layers[i]->updateGradient(A,0.0);
	 sseA = calcError(tin, tout);
	 for (i=0;i<layers.size();i++)
	    layers[i]->loadWeights();

	 for (i=0;i<layers.size();i++)
	    layers[i]->saveWeights();
	 for (i=0;i<layers.size();i++)
	    layers[i]->updateGradient(B,0.0);
	 sseB = calcError(tin, tout);
	 for (i=0;i<layers.size();i++)
	    layers[i]->loadWeights();
	 middle = sqrt(A*B);
	 //cerr << A << ": " << sseA << "\t" << B << ": " << sseB << "\t" << middle << endl;
	 if (sseA > sseB)
	    A=middle;
	 else
	    B=middle;
      }
      for (i=0;i<layers.size();i++)
	 layers[i]->updateGradient(middle,0.0);
      min_error = calcError(tin, tout);

      iter--;


      cout << (min_error/tin.size()/topo[topo.size()-1]) << "\t" << middle << "\t" << tin.size() << endl;


   }
}


void FFNet::trainDeltaBar(vector<float *> tin, vector<float *> tout, int iter, double learnRate, 
			  double mom, double increase, double decrease, int nbSets)
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

   Array<double> alpha(nbWeights);
   Array<double> wk(nbWeights);
   Array<double> nextW(nbWeights);
   Array<double> dEk(nbWeights);
   Array<double> nextdE(nbWeights);
   double nextE;

   getWeights(wk.begin());

   for (i=0;i<nbWeights;i++)
      alpha[i] = learnRate;

   calcGradient(tin, tout, wk, dEk, SSE);
   while (iter--)
   {

      double norm = dEk.norm();
      double norm_1 = 1/norm;
      
      for (i=0;i<nbWeights;i++)
	 nextW[i] = wk[i] - alpha[i] * norm_1 * dEk[i];

      calcGradient(tin, tout, nextW, nextdE, nextE);

      if (nextE > SSE)
      {
	 alpha *= decrease;
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
	 if (alpha[i] < .00000000001)
	    alpha[i] = .00000000001;
      }
      cout << (SSE/tin.size()/topo[topo.size()-1]) << "\t" << tin.size() << endl;
      SSE=nextE;
      dEk = nextdE;
      wk = nextW;

   }
   setWeights(wk.begin());
}


/*
void FFNet::trainDeltaBar(vector<float *> tin, vector<float *> tout, int iter, double learnRate, 
			  double mom, double increase, double decrease, int nbSets)
{

   int i,j;
   //double in[topo[0]];
   //double out[topo[topo.size()-1]];
   
   //we should get 8-byte alignment
   double *in = new double [topo[0]];
   double *out = new double [topo[topo.size()-1]];

   for (i=0;i<layers.size();i++)
   {
      layers[i]->setDeltaRate(learnRate);
   }

   while (iter)
   {

      double SSE = 0;

      for (int batchSet=0; batchSet < nbSets; batchSet++)
      {
	 for (i=0;i<layers.size();i++)
	 {
	    layers[i]->resetGradient();
	 }
	 for (i=batchSet;i<tin.size();i+=nbSets)
	 {
	    //double in[topo[0]];
	    //double out[topo[topo.size()-1]];
	    for (j=0;j<topo[0];j++)
	       in[j]=tin[i][j];
	    for (j=0;j<topo[topo.size()-1];j++)
	       out[j]=tout[i][j];
	    learn (in, out, &SSE);
	    
	 }

	 for (i=0;i<layers.size();i++)
	 {
	    layers[i]->deltaBar(mom, increase, decrease);
	    //layers[i]->updateGradient(alpha,momentum);
	 }

      }

      iter--;

      cout << (SSE/tin.size()/topo[topo.size()-1]) << "\t" << tin.size() << endl;


   }

   delete [] in;
   delete [] out;
}
*/


void FFNet::trainRecurrent(vector<float *> tin, vector<float *> tout, int iter, double learnRate, 
			  double mom, double increase, double decrease)
{

   int i,j;
   //double in[topo[0]];
   //double out[topo[topo.size()-1]];
   
   //we should get 8-byte alignment
   double *in = new double [topo[0]];
   double *out = new double [topo[topo.size()-1]];
   double *old_out = new double [topo[topo.size()-1]];

   for (i=0;i<layers.size();i++)
   {
      layers[i]->setDeltaRate(learnRate);
   }

   while (iter)
   {

      double SSE = 0;

      for (i=0;i<layers.size();i++)
      {
	 layers[i]->resetGradient();
      }
      
      for (j=0;j<topo[topo.size()-1];j++)
	 old_out[j]=0;
      
      for (i=0;i<tin.size();i++)
      {
	 int inSize = topo[0] - topo[topo.size()-1];
	 for (j=0;j<inSize;j++)
	    in[j]=tin[i][j];
	 for (j=inSize;j<topo[topo.size()-1];j++)
	    in[j]=old_out[j-inSize];
	 for (j=0;j<topo[topo.size()-1];j++)
	    out[j]=tout[i][j];
	 learn (in, out, &SSE, old_out);
	 
      }
      
      for (i=0;i<layers.size();i++)
      {
	 layers[i]->deltaBar(mom, increase, decrease);
	 //layers[i]->updateGradient(alpha,momentum);
      }
      

      iter--;

      cout << (SSE/tin.size()/topo[topo.size()-1]) << "\t" << tin.size() << endl;


   }

   delete [] in;
   delete [] out;
}


/* This algorithm is incomplete
void FFNet::learnlm(double *input, double *output, double **jacob, double *err, double &sse)
{
   int outputLayer = topo.size()-2;
   calc(input);

   int woffset1=0;
   int woffset2=0;
   int prev_offset=0;
   //start with the output layer, towards the input

   for (int k=outputLayer;k>=0;k--)
   {
      FFLayer *currentLayer = layers[k];
      double *previousValue, *currentValue;
      if (k==0)
	 previousValue = input;
      else 
	 previousValue = layers[k-1]->getValue();

      currentValue = currentLayer->getValue();

      int layerSize = topo[k+1];
      int layerInputs = topo[k];
      //double *delta = currentLayer->getError();


      if (k==outputLayer)
      {
	 woffset2=woffset1;
	 //double *w = currentLayer->getTmpWeights(i);
	 for (int ei=0;ei<layerSize;ei++)
	 {
	    err[ei] = currentValue[ei]-output[ei];
	    sse += (currentValue[ei]-output[ei])*(currentValue[ei]-output[ei]);

	    for (int wi=0;wi<layerInputs;wi++)
	       jacob[ei][woffset2+wi] = currentLayer->deriv[ei]*previousValue[wi];
	    jacob[ei][woffset2+layerInputs] = currentLayer->deriv[ei];
	    woffset2+=layerInputs+1;
	 }
	 woffset1 += (layerInputs+1)*layerSize;
	 
      } else {
	 //FFLayer *nextLayer = layers[k+1];
	 woffset2=woffset1;
	 //double *w = currentLayer->getTmpWeights(i);
	 for (int ni=0;ni<layerSize;ni++)
	 {
	    for (int ei=0;ei<topo[outputLayer];ei++)
	    {
	       double contrib=0;
	       for (int t = prev_offset + ni ; t < woffset1 ; t += layerSize+1)
		  contrib += jacob[ei][t];

	       for (int wi=0;wi<layerInputs;wi++)
		  jacob[ei][woffset2+wi] = contrib*currentLayer->deriv[ni]*previousValue[wi];
	       jacob[ei][woffset2+layerInputs] = contrib*currentLayer->deriv[ni];
	    }
	    woffset2+=layerInputs+1;
	 }
	 prev_offset=woffset1;
	 woffset1 += (layerInputs+1)*layerSize;


      }
   }	 
}


void FFNet::trainlm(vector<float *> tin, vector<float *> tout, int maxIter)
{
   int nb_outputs = topo[topo.size()-1];
   double **jacob = new double * [nb_outputs];

   int nb_weights=0;
   for (int i=0;i<topo.size()-1;i++)
      nb_weights += (topo[i]+1)*topo[i+1];

   for (int i=0;i<nb_outputs;i++)
      jacob[i] = new double [nb_weights];

   double **jacob2 = new double * [nb_weights];
   for (int i=0;i<nb_weights;i++)
      jacob2[i] = new double [nb_weights];


   for (int iter=0; iter<maxIter; iter++)
   {
      double sse=0;

      //initialize jacobi
      for (int i=0;i<nb_outputs;i++)
	 for (int j=0;j<nb_weights;j++)
	    jacob[i][j]=0;
      
      //initializes error
      double err[nb_outputs];
      for (int i=0;i<nb_outputs;i++)
	 err[i]=0;

      //initialize gradient
      double grad[nb_weights];
      for (int i=0;i<nb_weights;i++)
	 grad[i]=0;

      //iterate on all data
      for (int i=0;i<tin.size();i++)
	 //for (int i=0;i<tin.size();i+=100)
      {
	 for (int j=0;j<nb_outputs;j++)
	    for (int k=0;k<nb_weights;k++)
	       jacob[j][k]=0;
	 

	 double in[topo[0]];
	 double out[topo[topo.size()-1]];
	 for (int j=0;j<topo[0];j++)
	    in[j]=tin[i][j];
	 for (int j=0;j<topo[topo.size()-1];j++)
	    out[j]=tout[i][j];
	 learnlm (in, out, jacob, err, sse);
	 
	 for (int j=0;j<nb_weights;j++)
	    for (int k=0;k<nb_outputs;k++)
	    grad[j]+=jacob[k][j]*err[k];
	 
      }

      for (int i=0;i<nb_outputs;i++)
      {
	 for (int j=0;j<nb_weights;j++)
	    cerr << jacob[i][j] << " ";
	 cerr << endl;
	 }

      //calculate gradient
      for (int i=0;i<nb_weights;i++)
	 for (int j=0;j<nb_outputs;j++)
	    grad[i] += jacob[j][i]*err[j];
      

      double delta[nb_weights];
      for (int i=0;i<nb_weights;i++)
	 delta[i] = -.0000001*grad[i];

      for (int i=0;i<nb_outputs;i++)
	 cerr << err[i] << " ";
      cerr << endl;
      for (int i=0;i<nb_weights;i++)
	 cerr << grad[i] << " ";
      cerr << endl;
      for (int i=0;i<nb_weights;i++)
	 cerr << delta[i] << " ";
      cerr << endl;

      int offset=0;
      int outputLayer = topo.size()-2;
      for (int k=outputLayer;k>=0;k--)
      {
	 FFLayer *currentLayer = layers[k];
	 int layerSize = topo[k+1];
	 int layerInputs = topo[k];
	 for (int ni=0;ni<layerSize;ni++)
	 {
	    double *w = currentLayer->getWeights(ni);
	    for (int wi=0;wi<layerInputs;wi++)
	    {
	       w[wi]+=delta[offset+wi];
	    }
	    w[layerInputs]+=delta[offset+layerInputs];
	    offset += layerInputs+1;
	 }	 
      }

      cout << (sse/tin.size()/topo[topo.size()-1]) << endl;

   }


   for (int i=0;i<nb_outputs;i++)
      delete [] jacob[i];
   delete [] jacob;

   for (int i=0;i<nb_weights;i++)
      delete [] jacob2[i];
   delete [] jacob2;

}
*/
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
}

istream &operator >> (istream &in, FFNet &net)
{
   if (!isValidType(in, "FFNet")) return in;
   net.readFrom(in);
   return in;
}
