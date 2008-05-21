// Copyright (C) 2001 Jean-Marc Valin

#include "FFLayer.h"
#include <stdlib.h>
#include "ObjectParser.h"
#include "Vector.h"
#include "misc.h"

using namespace std;

namespace FD {

DECLARE_TYPE(FFLayer)
DECLARE_TYPE(Vector<FFLayer>)
DECLARE_TYPE(Vector<FFLayer*>)
DECLARE_TYPE2("Vector<RCPtr<FFLayer>>", Vector<RCPtr<FFLayer> >)

//@implements FFNet

FFLayer::FFLayer (int _nbNeurons, int _nbInputs, float *_weights, int _weightOffset, int _neuronOffset, string type)
   : nbNeurons(_nbNeurons)
   , nbInputs (_nbInputs)
   , funcType(type)
   , weights(_weights+_weightOffset)
   , weightOffset (_weightOffset)
   , neuronOffset (_neuronOffset)
   , derivOffset(0)
{
   if (funcType == "lin")
   {
      func = lin;
      deriv_func = deriv_lin;
   } else if (funcType == "sigmoid")
   {
      func = sigmoid;
      deriv_func = deriv_sigmoid;
   } else if (funcType == "tansig")
   {
      func = tansig;
      deriv_func = deriv_tansig;
   }
}

void FFLayer::setupAfterRead(float *_weights, int _weightOffset, int _neuronOffset)
{
   weightOffset=_weightOffset;
   neuronOffset=_neuronOffset;
   //cerr << "offsets: " << weightOffset << " " << neuronOffset << endl;
   float *tmp = weights;
   weights = _weights+_weightOffset;
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
      weights[i] = tmp[i];
   delete [] tmp;
}


void FFLayer::init(float minmax)
{
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
   {
      //weights[i]=1.0;
      weights[i]=gauss_rand(sqrt(1.0/nbInputs))/minmax;
   }
}

void FFLayer::init(double *mean, double *std)
{
   for (int i=0;i<nbNeurons;i++)
   {
      float meanSum = 0;
      for (int j=0;j<nbInputs;j++)
      {
	 weights[i*(nbInputs+1) + j] = gauss_rand(sqrt(1.0/nbInputs)/(1e-5+std[j]));
	 meanSum += weights[i*(nbInputs+1) + j] * mean[j];
      }
      weights[i*(nbInputs+1) + nbInputs] = gauss_rand(sqrt(1.0/nbInputs)) - meanSum;
   }
}

void FFLayer::setBias(double *minmax)
{
   for (int i=0;i<nbNeurons;i++)
   {
      //cerr << minmax[i] << " ";
      weights[i*(nbInputs+1) + nbInputs] = minmax[i];
   }   
   //cerr << endl;
}

void FFLayer::printOn(ostream &out) const
{
   out << "<FFLayer " << endl;
   out << "<nbNeurons " << nbNeurons << ">" << endl;
   out << "<nbInputs " << nbInputs << ">" << endl;
   out << "<funcType " << funcType << " >" << endl;

   out << "<weights ";
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
      out << weights[i] << " ";
   out << " >" << endl;

   out << ">\n";
}


void FFLayer::readFrom (istream &in)
{
   string tag;
   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("FFLayer::readFrom : Parse error: '<' expected");
      in >> tag;
      //cerr << "layer tag = " << tag << endl;
      if (tag == "nbNeurons")
         in >> nbNeurons;
      else if (tag == "nbInputs")
         in >> nbInputs;
      else if (tag == "funcType")
      {
         in >> funcType;
	 if (funcType == "lin")
	 {
	    func = lin;
	    deriv_func = deriv_lin;
	 } else if (funcType == "sigmoid")
	 {
	    func = sigmoid;
	    deriv_func = deriv_sigmoid;
	 } else if (funcType == "tansig")
	 {
	    func = tansig;
	    deriv_func = deriv_tansig;
	 } /*else if (funcType == "tanh")
	 {
	    func = tanh;
	    deriv_func = deriv_tanh;
	    }*/
      } else if (tag == "weights")
      {
	 weights = new float [nbNeurons*(nbInputs+1)];
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    in >> weights[i];
	 }
      }
      else
         throw new ParsingException ("FFLayer::readFrom : unknown argument: " + tag);

      if (!in) throw new ParsingException ("FFLayer::readFrom : Parse error trying to build " + tag);

      in >> tag;
      //cerr << "end tag = " << tag << endl;
      if (tag != ">") 
         throw new ParsingException ("FFLayer::readFrom : Parse error: '>' expected ");
   }
   //cerr << "done with layer\n";
}


istream &operator >> (istream &in, FFLayer &layer)
{
   if (!isValidType(in, "FFLayer")) return in;
   layer.readFrom(in);
   return in;
}
}//namespace FD
