#include <math.h>
#include <vector>
#include <iostream>
#include "Object.h"
#include "FFNet.h"


class NNetSet;

ostream &operator << (ostream &out, const NNetSet &cell);


class NNetSet : public Object {
protected:
   vector<FFNet *> nets;
public:
   //NNetSet() 
   //{}

   NNetSet(){}
   
   NNetSet (const NNetSet &) {cerr << "don't call the NNetSet copy constructor\n"; exit(1);}

   NNetSet(int nbNets, const Vector<int> &topo, const vector<string> &functions, vector<int> id, vector<float *> &tin, vector<float *> &tout);
   
   NNetSet(vector<int> id, vector<float *> &tin, vector<float *> &tout, NNetSet *net1, NNetSet *net2);

   ~NNetSet() 
   {
   }

   double *calc(int id, const double *input);

   void train(vector<int> id, vector<float *> tin, vector<float *> tout, int iter, 
	      double learnRate, double mom, double increase, double decrease, double errRatio, int nbSets);

   void trainDeltaBar(vector<int> id, vector<float *> tin, vector<float *> tout, 
		      int iter, double learnRate, double mom, double increase, 
		      double decrease, int nbSets);

   void trainCGB(vector<int> id, vector<float *> tin, vector<float *> tout, 
		      int iter, double sigma = .03, double lambda = .2);

   void printOn(ostream &out) const;

   void readFrom (istream &in);

   friend istream &operator >> (istream &in, NNetSet &cell);
};

