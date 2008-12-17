// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <stdlib.h>
#include <math.h>
#include "macros_math.h"

using namespace std;

namespace FD {

class Constellation;

DECLARE_NODE(Constellation)
/*Node
 *
 * @name Constellation
 * @category Communications:Transmitter
 * @description Generate a sequence of constellation points
 *
 * @input_name INPUT
 * @input_type Vector<int>
 * @input_description Binary input sequence.
 *
 * @output_name OUTPUT
 * @output_type Vector<complex<float> >
 * @output_description Generated sequence.
 *
 * @parameter_name TYPE
 * @parameter_type string
 * @parameter_description Modulation type (PAM, PSK, QAM, FILE).
 * @parameter_value PSK
 *
 * @parameter_name NBITS
 * @parameter_type int
 * @parameter_description Number of bits per symbol.
 * @parameter_value 1
 *
 * @parameter_name ENERGY
 * @parameter_type float
 * @parameter_description Average symbol energy per complex dimension.
 * @parameter_value 1.
 *
 * @parameter_name FILENAME
 * @parameter_type string
 * @parameter_description Name of file containing the constellation.
 * @parameter_value Xi.txt
END*/


class Constellation : public BufferedNode {
   
	int inputID;
	int outputID;
	int nbits;
	int nsignals;
	float Es;
	enum modType {PAM, PSK, QAM, FILE_T};
	modType type;
	RCPtr<Vector<complex<float> > > Xiptr;
	string filename;

public:
Constellation(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	int i, j;
	int nbh, nbhp;
	unsigned int ui;

	inputID = addInput("INPUT");
	outputID = addOutput("OUTPUT");

      if (parameters.exist("TYPE"))
      {
	if (object_cast<String> (parameters.get("TYPE")) == "PAM")
	    type = PAM;
	else if (object_cast<String> (parameters.get("TYPE")) == "PSK")
	    type = PSK;
	else if (object_cast<String> (parameters.get("TYPE")) == "QAM")
	    type = QAM;
	else if (object_cast<String> (parameters.get("TYPE")) == "FILE")
	    type = FILE_T;
	else
	    new NodeException(NULL, "Unknown function type", __FILE__, __LINE__);
      } else type = PAM;

      if (parameters.exist("NBITS"))
	 nbits = dereference_cast<int> (parameters.get("NBITS"));
      else
	 nbits = 1;

      nsignals = 1 << nbits;

      if (parameters.exist("ENERGY"))
	 Es = dereference_cast<float> (parameters.get("ENERGY"));
      else
	 Es = 1.;


	if(parameters.exist("FILENAME"))
	{
		filename = object_cast<String> (parameters.get("FILENAME"));
	}

	Xiptr = Vector<complex<float> >::alloc(1 << nbits);
	Vector<complex<float> > &Xi = *Xiptr;

	complex<float>  tmpc;
	double Es0 = 0.;
	ifstream ifs;
	switch(type)
	{
		case PAM:
			for (i = 0; i < nsignals; i++)
			{
				Xi[BIN2GRAY(i)] = complex<float>(nsignals - 1 - 2 * i, 0.);
				Xi[BIN2GRAY(i)] *= sqrt((3. * Es) / float(nsignals * nsignals - 1.));
			}
			break;
		case PSK:
			for (i = 0; i < nsignals; i++)
			{
				Xi[BIN2GRAY(i)] = complex<float>(cos((1 + 2 * i) * PI / float(nsignals)),
						sin((1 + 2 * i) * PI / float(nsignals)));
				Xi[BIN2GRAY(i)] *= sqrt(Es);
			}
			break;
		case QAM:
			nbh = 1 << (nbits >> 1);
			nbhp = 1 << ((nbits + 1) >> 1);
			for(i = 0; i < nbhp; i++)
			{
				for(j = 0; j < nbh; j++)
				{
					Xi[BIN2GRAY(j) + nbh * BIN2GRAY(i)] =
						complex<float>(nbhp - 1 - 2 * i, nbh - 1 - 2 * j);
					Es0 += Xi[BIN2GRAY(j) + nbh * BIN2GRAY(i)].real() *
						Xi[BIN2GRAY(j) + nbh * BIN2GRAY(i)].real();
					Es0 += Xi[BIN2GRAY(j) + nbh * BIN2GRAY(i)].imag() *
						Xi[BIN2GRAY(j) + nbh * BIN2GRAY(i)].imag();
				}
			}
			Es0 /= float(nsignals);
			for(i = 0; i < nbhp; i++)
			{
				for(j = 0; j < nbh; j++)
				{
					Xi[BIN2GRAY(j) + nbh * BIN2GRAY(i)] *= sqrt(Es / Es0);
				}
			}
			break;
		case FILE_T:
			ifs.open(filename.c_str());
			ifs >> Xi;
			ifs.close();

			for(ui = 0; ui < Xi.size(); ui++)
			{
				Es0 += Xi[ui].real() * Xi[ui].real();
				Es0 += Xi[ui].imag() * Xi[ui].imag();
			}
			nsignals = Xi.size();
			nbits = floor(log2(nsignals));
			if(nsignals > (1 << nbits))
				throw new NodeException(this, "Constellation loaded from file has non power of 2 size.", __FILE__, __LINE__);
			Es0 /= float(nsignals);
			for(i = 0; i < nsignals; i++)
			{
				Xi[i] *= sqrt(Es / Es0);
			}
			break;
		default:
			throw new NodeException(this, "Invalid constellation type.", __FILE__, __LINE__);
	}
}



void calculate(int output_id, int count, Buffer &out)
{
	int i, j, k;
	
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<int> &inputVec = object_cast<Vector<int> >(inputRef);
	int length = inputVec.size();

	if(type != FILE_T && (length % nbits)) throw new NodeException(this, "Constellation: input length % nbits != 0", __FILE__, __LINE__);

	Vector<complex<float> > &output = *Vector<complex<float> >::alloc(length / nbits);

	out[count] = &output;
	Vector<complex<float> > &Xi = *Xiptr;

	for (i = 0; i < length / nbits; i++)
	{
		k = 0;				// TODO: add nonbinary input
		for(j = 0; j < nbits; j++)
		{
			k += (inputVec[i * nbits + j] & 1) << j;
		}
		output[i] = Xi[k];
	}

}
};

}//namespace FD
