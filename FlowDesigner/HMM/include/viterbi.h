// Copyright (C) 1999 Jean-Marc Valin
#ifndef VITERBI_H
#define VITERBI_H

namespace FD {

class ViterbiNode {
   
}

class ViterbiGraph {

protected:

   int numberStates;
   int numberFrames;
   
   vector <State *> states;
   vector <Vector<float> *> frames;
   

public:

};
}//namespace FD
#endif
