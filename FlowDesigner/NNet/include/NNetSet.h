// Copyright (C) 2001 Jean-Marc Valin

#include <math.h>
#include <vector>
#include <iostream>
#include "Object.h"
#include "FFNet.h"


class NNetSet;

std::ostream &operator << (std::ostream &out, const NNetSet &cell);


class NNetSet : public Object {
protected:
   std::vector<RCPtr<FFNet> > nets;
   float *value;
public:
   //NNetSet() 
   //{}

   NNetSet(){value = NULL;}
   
   NNetSet (const NNetSet &) {std::cerr << "don't call the NNetSet copy constructor\n"; exit(1);}

   NNetSet(int nbNets, const Vector<int> &topo, const Vector<std::string> &functions, std::vector<int> id, std::vector<float *> &tin, std::vector<float *> &tout);
   
   NNetSet(std::vector<int> id, std::vector<float *> &tin, std::vector<float *> &tout, NNetSet *net1, NNetSet *net2);

   ~NNetSet() 
   {
      delete [] value;
   }

   float *calc(int id, const float *input);

   //void train(std::vector<int> id, std::vector<float *> tin, std::vector<float *> tout, int iter, 
//	      double learnRate, double mom, double increase, double decrease, double errRatio, int nbSets);

   void trainDeltaBar(std::vector<int> id, std::vector<float *> tin, std::vector<float *> tout, 
		      int iter, double learnRate, double increase, double decrease);

   //void trainCGB(std::vector<int> id, std::vector<float *> tin, std::vector<float *> tout, 
   //	      int iter, double sigma = .03, double lambda = .2);

   void printOn(std::ostream &out) const;

   void readFrom (std::istream &in);

   friend std::istream &operator >> (std::istream &in, NNetSet &cell);
};

