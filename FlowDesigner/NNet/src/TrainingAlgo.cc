// Copyright (C) 2001 Jean-Marc Valin


#include "TrainingAlgo.h"
#include "FFNet.h"
#include <vector>
#include "Vector.h"

using namespace std;

//@require FFNet
//@implements FFNetTrain

void TrainingDeltaBarDelta::train(FFNet *net, vector<float *> tin, vector<float *> tout, 
				  int iter, float learnRate, float increase, float decrease, 
				  int nbSets, bool rprop)
{
   int i,j;
   double SSE;
   int k=1;

   int nbWeights = 0;
   cerr << "training\n";
   const float *weights = net->getWeights();
   const Vector<RCPtr<FFLayer> > &layers=net->getLayers();
   const Vector<int> &topo = net->getTopo();


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
   Array<double> tmpdE(nbWeights);
   double nextE;
   double tmpE;

   vec_copy(weights, &wk[0], nbWeights);

   for (i=0;i<nbWeights;i++)
      alpha[i] = learnRate;

   for (i=0;i<nbWeights;i++)
      tmpdE[i]=0;
   tmpE=0;
   net->calcGradient(tin, tout, wk, dEk, SSE);
   while (iter--)
   {

      //float norm = dEk.norm();
      float norm_1 = 1;
      if (!rprop)
	 norm_1 = 1.0/tin.size();// / norm;
      
      if (rprop)
      {
	 for (i=0;i<nbWeights;i++)
	 {
	    if (dEk[i]>0)
	       dEk[i] = 1;
	    else if (dEk[i]<0)
	       dEk[i] = -1;
	    else 
	       dEk[i] = 0;
	 }
      }
      
      if (nbSets ==1)
      {
	 for (i=0;i<nbWeights;i++)
	    nextW[i] = wk[i] - alpha[i] * norm_1 * dEk[i];
	 net->calcGradient(tin, tout, nextW, nextdE, nextE);
      } else {
	 for (i=0;i<nbWeights;i++)
	    nextW[i] = wk[i];
	 
	 nextE=0;
	 for (i=0;i<nbWeights;i++)
	    nextdE[i] = 0;
	 
	 vector<int> remaining(tin.size());
	 for (i=0;i<tin.size();i++)
	    remaining[i]=i;
	 vector<float *> batchIn;
	 vector<float *> batchOut;
	 for (i=0;i<nbSets;i++)
	 {
	    batchIn.resize(0);
	    batchOut.resize(0);
	    for (j=0;j<nbWeights;j++)
	       nextW[j] -= alpha[j] * norm_1 * tmpdE[j];
	    
	    int putBack=0;
	    for (j=0;j<remaining.size();j++)
	    {
	       int id = remaining[j];
	       if (rand()%(nbSets-i) == 0)
	       {
		  batchIn.push_back(tin[id]);
		  batchOut.push_back(tout[id]);
	       } else {
		  remaining[putBack++] = id;
	       }
	    }
	    remaining.resize(putBack);
	    net->calcGradient(batchIn, batchOut, nextW, tmpdE, tmpE);
	    nextE+=tmpE;
	    for (j=0;j<nbWeights;j++)
	       nextdE[j] += tmpdE[j];
	 }
      }
      
      if (nextE > SSE)
      {
	 alpha *= decrease;
	 //So that the "bad" iteration doesn't count
	 iter++;
	 cerr << "backing off\n";
	 /*for (i=0;i<nbWeights;i++)
	   tmpdE[i]=0;*/
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

   net->setWeights(&wk[0]);
   //vec_copy(&wk[0], net->weights, nbWeights);
}





void TrainingSCG::train(FFNet *net, vector<float *> tin, vector<float *> tout, int iter, float sigma, float lambda)
{


   int i,j;
   double SSE;
   int k=1;
   float lambda_init = lambda;
   float lambdaBar = 0;
   float sigmak;
   bool success = true;

   int nbWeights = 0;

   const float *weights = net->getWeights();
   const Vector<RCPtr<FFLayer> > &layers=net->getLayers();
   const Vector<int> &topo = net->getTopo();

   for (i=0;i<layers.size();i++)
   {
      nbWeights += layers[i]->getNbWeights();
   }


   cerr << "WARNING: This is still experimental" << endl;
   cerr << "found " << nbWeights << " weights\n";


   Array<float> pk(nbWeights);
   Array<float> rk(nbWeights);
   Array<float> sk(nbWeights);
   Array<float> wk(nbWeights);
   Array<float> dEk(nbWeights);
   Array<float> dEp(nbWeights);
   Array<float> nextdE(nbWeights);

   Array<double> tmp(nbWeights);

   double nextE;
   float deltak;

   vec_copy(weights, &wk[0], nbWeights);


   net->calcGradient(tin, tout, wk, tmp, SSE);
   for (int i=0;i<nbWeights;i++)
      dEk[i] = tmp[i];



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
	 double dummy = 0;

	 net->calcGradient(tin, tout, wk+pk*sigmak, tmp, dummy);
	 for (int i=0;i<nbWeights;i++)
	    dEp[i] = tmp[i];


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
      net->calcGradient(tin, tout, wk+pk*ak, tmp, nextE);
      for (int i=0;i<nbWeights;i++)
	 nextdE[i] = tmp[i];

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
   net->setWeights(&wk[0]);
}


void TrainingQProp::train(FFNet *net, vector<float *> tin, vector<float *> tout, int iter, float learnRate)
{

   int i,j;
   double SSE;
   int k=1;

   float increase=1.04;
   float decrease = .6;

   int nbWeights = 0;

   const float *weights = net->getWeights();
   const Vector<RCPtr<FFLayer> > &layers=net->getLayers();
   const Vector<int> &topo = net->getTopo();

   for (i=0;i<layers.size();i++)
   {
      nbWeights += layers[i]->getNbWeights();
   }
   cerr << "WARNING: This implementation of Quickprop doesn't work yet!" << endl;
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

   net->calcGradient(tin, tout, wk, dE, SSE);
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

      net->calcGradient(tin, tout, nextW, nextdE, nextE);

      while(nextE > SSE)
      {
	 float alpha = learnRate;
	 for (i=0;i<nbWeights;i++)
	    nextW[i] = wk[i] - alpha*dE[i];
	 net->calcGradient(tin, tout, nextW, nextdE, nextE);
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

   net->setWeights(&wk[0]);
   //vec_copy(&wk[0], weights, nbWeights);


}



void TrainingWeightDeltaBarDelta::train(FFNet *net, vector<float *> tin, vector<float *> tout, vector<float *> learnWeights, int iter, float learnRate, 
			 float increase, float decrease)
{
   int i,j;
   double SSE;
   int k=1;

   int nbWeights = 0;

   const float *weights = net->getWeights();
   const Vector<RCPtr<FFLayer> > &layers=net->getLayers();
   const Vector<int> &topo = net->getTopo();


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

   net->weightedCalcGradient(tin, tout, learnWeights, wk, dEk, SSE);
   while (iter--)
   {

      float norm = dEk.norm();
      float norm_1 = 1;// / norm;
      
      for (i=0;i<nbWeights;i++)
	 nextW[i] = wk[i] - alpha[i] * norm_1 * dEk[i];
      net->weightedCalcGradient(tin, tout, learnWeights, nextW, nextdE, nextE);
      
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
	 if (alpha[i] < 1e-58)
	    alpha[i] = 1e-58;
      }

      //if (SSE/tin.size()/topo[topo.size()-1]<.08) break;
      cout << (SSE/tin.size()/topo[topo.size()-1]) << "\t" << tin.size() << endl;
      SSE=nextE;
      dEk = nextdE;
      wk = nextW;

   }

   net->setWeights(&wk[0]);
   //vec_copy(&wk[0], net->weights, nbWeights);
}
