#include "FFLayer.h"
#include <stdlib.h>
#include "ObjectParser.h"

//DECLARE_TYPE(FFLayer)




FFLayer::FFLayer (int _nbNeurons, int _nbInputs, float *_weights, int _weightOffset, int _neuronOffset, string type)
   : nbNeurons(_nbNeurons)
   , nbInputs (_nbInputs)
   , funcType(type)
   , weights(_weights+_weightOffset)
   , weightOffset (_weightOffset)
   , neuronOffset (_neuronOffset)
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

/*
FFLayer::FFLayer(const FFLayer &layer)
   : nbNeurons(layer.nbNeurons)
   , nbInputs(layer.nbInputs)
   , funcType(layer.funcType)
   , func(layer.func)
   , deriv_func(layer.deriv_func)
   , alloc(true)
{

   weights = new float [nbNeurons*(nbInputs+1)];

   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
      weights[i] = layer.weights[i];

   momentum = new float [nbNeurons*(nbInputs+1)];

   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
      momentum[i]=0;
   
   gradient = new float [nbNeurons*(nbInputs+1)];

   saved_weights = new float [nbNeurons*(nbInputs+1)];
   
   deriv = new float [nbNeurons];
   value = new float [nbNeurons];
   error = new float [nbNeurons];
}
*/


void FFLayer::init(float minmax)
{
   for (int i=0;i<nbNeurons*(nbInputs+1);i++)
   {
      //weights[i]=1.0;
      weights[i]=sqrt(3.0/nbInputs)*((rand()%1000) * .002 - .1)/minmax;
   }
}

void FFLayer::init(float *mean, float *std)
{
   float meanSum = 0;
   for (int j=0;j<nbInputs;j++)
      meanSum += mean[j];
   for (int i=0;i<nbNeurons;i++)
   {
      float meanSum = 0;
      for (int j=0;j<nbInputs;j++)
      {
	 weights[i*(nbInputs+1) + j] = sqrt(3.0/nbInputs)*((rand()%1000) * .002 - .1)/std[j];
	 meanSum += weights[i*(nbInputs+1) + j]*mean[j];
      }
      weights[i*(nbInputs+1) + nbInputs] = sqrt(3.0/nbInputs)*((rand()%1000) * .002 - .1) - meanSum;
   }
}

void FFLayer::setBias(float *minmax)
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
       throw new ParsingException ("Parse error: '<' expected");
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
         throw new ParsingException ("unknown argument: " + tag);

      if (!in) throw new ParsingException ("Parse error trying to build " + tag);

      in >> tag;
      //cerr << "end tag = " << tag << endl;
      if (tag != ">") 
         throw new ParsingException ("Parse error: '>' expected ");
   }
   //cerr << "done with layer\n";
}


istream &operator >> (istream &in, FFLayer &layer)
{
   if (!isValidType(in, "FFLayer")) return in;
   layer.readFrom(in);
   return in;
}
