#include <math.h>
#include <vector>
#include <stream.h>
#include "Object.h"
#include "kmeans.h"
#include "Vector.h"

class CodebookMap;

ostream &operator << (ostream &out, const CodebookMap &cell);


class CodebookMap : public Object {
protected:
   KMeans mapIn;
   
   Vector<Vector<float> > mapOut;

public:
   //CodebookMap() 
   //{}

   CodebookMap(){}
   
   CodebookMap (const CodebookMap &) {cerr << "don't call the CodebookMap copy constructor\n"; exit(1);}

   CodebookMap(const KMeans &_mapIn, const vector<float *> dataIn, const vector<float *> dataOut, int length);

   ~CodebookMap()
   {
   }

   
   const float * calcOutput(const float *in) const;

   void printOn(ostream &out) const;

   void readFrom (istream &in);

   friend istream &operator >> (istream &in, CodebookMap &cell);
};

