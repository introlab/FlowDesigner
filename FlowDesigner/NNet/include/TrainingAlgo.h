// Copyright (C) 2001 Jean-Marc Valin

#ifndef TRAINING_ALGO_H
#define TRAINING_ALGO_H

#include <vector>

class FFNet;

class TrainingAlgo {
  public:
};



class TrainingDeltaBarDelta : public TrainingAlgo {
  public:
   static void train(FFNet *net, vector<float *> tin, vector<float *> tout, int iter, float learnRate, 
	      float increase, float decrease, int nbSets=1);
};


class TrainingSCG : public TrainingAlgo {
  public:
   static void train(FFNet *net, vector<float *> tin, vector<float *> tout, int iter, float sigma, float lambda);
};

class TrainingQProp : public TrainingAlgo {
  public:
   static void train(FFNet *net, vector<float *> tin, vector<float *> tout, int iter, float learnRate);
};

class TrainingWeightDeltaBarDelta : public TrainingAlgo {
  public:
   static void train(FFNet *net, vector<float *> tin, vector<float *> tout, vector<float *> learnWeights, int iter, float learnRate, 
	      float increase, float decrease);
};

#endif
