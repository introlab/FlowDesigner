// Copyright (C) 1999 Jean-Marc Valin
#ifndef MODEL_H
#define MODEL_H

#include <vector>
#include "state.h"


/**Base model class, can be used for phonemes, allophones, ...*/
class Model {

   /**The model name*/
   string name;

   /**Set of states for the model*/
   vector<RCPtr<State> > states;

};


#endif
