// Copyright (C) 1999 Jean-Marc Valin
#ifndef STATE_H
#define STATE_H

#include "gmm.h"

namespace FD {

class Transition {
protected:
   /**The log probability of transition*/
   float probability;

   /**Transition going to ... (NULL means out)*/
   State *state;

public:
};


/**State class*/
class State {
   /**Corresponding GMM*/
   RCPtr<GMM> mixture;

   ///Log probability for transition to next state
   //vector<Transition> transitions;
   
};

}//namespace FD

#endif
