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

class LLRComputer;

DECLARE_NODE(LLRComputer)
/*Node
 *
 * @name LLRComputer
 * @category Communications:Receiver
 * @description Compute LLRs w.r.t. the points of the given constellation.
 *
 * @input_name INPUT
 * @input_type Vector<complex<float> >
 * @input_description Input sequence.
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Generated sequence.
 *
 * @parameter_name TYPE
 * @parameter_type string
 * @parameter_description Modulation type (PAM, PSK, QAM, FILE)
 * @parameter_value PSK
 *
 * @parameter_name NBITS
 * @parameter_type int
 * @parameter_description Number of bits per symbol. For QAM, number of bits per dimension.
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
 *
 * @parameter_name SIGMA
 * @parameter_type float
 * @parameter_description Standard deviation of noise.
 * @parameter_value 1.
 *
END*/


class LLRComputer : public BufferedNode {
   
	int inputID;
	int outputID;
	int nbits;
	int nsignals;
	float Es;
	// bool iscomplex;
	enum modType {PAM, PSK, QAM, FILE_T};
	modType type;
	RCPtr<Vector<complex<float> > > Xiptr;
	string filename;
	float sigma;

public:
LLRComputer(string nodeName, ParameterSet params)
: BufferedNode(nodeName, params)
{
	int i, j;
	unsigned int k;

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

      if (parameters.exist("ENERGY"))
	 Es = dereference_cast<float> (parameters.get("ENERGY"));
      else
	 Es = 1.;

	if (parameters.exist("SIGMA"))
		sigma = dereference_cast<float> (parameters.get("SIGMA"));
	else sigma = 1.;


	if(parameters.exist("FILENAME"))
	{
		filename = object_cast<String> (parameters.get("FILENAME"));
	}

	Xiptr = Vector<complex<float> >::alloc(1 << (nbits * ((type == QAM) ? 2 : 1)));
	Vector<complex<float> > &Xi = *Xiptr;

	complex<float>  tmpc;
	double Es0 = 0.;
	ifstream ifs;
	switch(type)
	{
		case PAM:
			nsignals = 1 << nbits;
			for (i = 0; i < nsignals; i++)
			{
				Xi[BIN2GRAY(i)] = complex<float>(nsignals - 1 - 2 * i, 0.);
				Xi[BIN2GRAY(i)] *= sqrt((3. * Es) / float(nsignals * nsignals - 1.));
				// ofs << BIN2GRAY(i) << Xi[BIN2GRAY(i)] << endl;
			}
			break;
		case PSK:
			nsignals = 1 << nbits;
			for (i = 0; i < nsignals; i++)
			{
				Xi[BIN2GRAY(i)] = complex<float>(cos((1 + 2 * i) * PI / float(nsignals)), sin((1 + 2 * i) * PI / float(nsignals)));
				Xi[BIN2GRAY(i)] *= sqrt(Es);
				// ofs << BIN2GRAY(i) << " " << Xi[BIN2GRAY(i)] << endl;
			}
			break;
		case QAM:
			nsignals = 1 << (nbits << 1);
			for(i = 0; i < (1 << nbits); i++)
			{
				for(j = 0; j < (1 << nbits); j++)
				{
					Xi[BIN2GRAY(j) + (1 << nbits) * BIN2GRAY(i)] = complex<float>((1 << nbits) - 1 - 2 * i, (1 << nbits) - 1 - 2 * j);
					Es0 += Xi[BIN2GRAY(j) + (1 << nbits) * BIN2GRAY(i)].real() * Xi[BIN2GRAY(j) + (1 << nbits) * BIN2GRAY(i)].real();
					Es0 += Xi[BIN2GRAY(j) + (1 << nbits) * BIN2GRAY(i)].imag() * Xi[BIN2GRAY(j) + (1 << nbits) * BIN2GRAY(i)].imag();
				}
			}
			Es0 /= float(nsignals);
			for(i = 0; i < (1 << nbits); i++)
			{
				for(j = 0; j < (1 << nbits); j++)
				{
					Xi[BIN2GRAY(j) + (1 << nbits) * BIN2GRAY(i)] *= sqrt(Es / Es0);
				}
			}
			break;
		case FILE_T:
			ifs.open(filename.c_str());
			ifs >> Xi;
			ifs.close();

			for(k = 0; k < Xi.size(); k++)
			{
				Es0 += Xi[k].real() * Xi[k].real();
				Es0 += Xi[k].imag() * Xi[k].imag();
			}
			nsignals = Xi.size();
			nbits = floor(log2(nsignals));
			// iscomplex = true;
			if(nsignals > (1 << nbits))
				throw new NodeException(this, "Constellation loaded from file has non power of 2 size", __FILE__, __LINE__);
			Es0 /= float(nsignals);
			for(i = 0; i < nsignals; i++)
			{
				Xi[i] *= sqrt(Es / Es0);
			}
			break;
		default:
			;
	}

}



void calculate(int output_id, int count, Buffer &out)
{

	int i, j;
	
	ObjectRef inputRef = getInput(inputID, count);
	const Vector<complex<float> > &inputVec = object_cast<Vector<complex<float> > >(inputRef);
	int length = inputVec.size();

	Vector<float> &output = *Vector<float>::alloc(length * nsignals);

	out[count] = &output;
	Vector<complex<float> > &Xi = *Xiptr;

	float yr, yi, mag;
	float sigma22 = 2. * sigma * sigma;
	for (i = 0; i < length; i++)
	{
		yr = inputVec[i].real();
		yi = inputVec[i].imag();
		mag = yr * yr + yi * yi;
		for(j = 0; j < nsignals; j++)
		{
			output[i * nsignals + j] = mag - (yr - Xi[j].real()) * (yr - Xi[j].real()) -
			       (yi - Xi[j].imag()) * (yi - Xi[j].imag());
			output[i * nsignals + j] /= sigma22;

		}
	}

}




};	// class LLRComputer

}	// namespace FD
