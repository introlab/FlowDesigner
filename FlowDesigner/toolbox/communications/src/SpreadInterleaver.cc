// Copyright (C) 2008 Alberto Perotti
// Author: Alberto Perotti

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "Interleaver.h"
#include "MersenneTwister.h"
#include "macros_math.h"

using namespace std;

namespace FD {

class SpreadInterleaver;

DECLARE_NODE(SpreadInterleaver)
/*Node
 *
 * @name SpreadInterleaver
 * @category Communications:Tools
 * @description Perform a pemutation of the input block. The generated  permutation satisfies given spread constraints.
 *
 * @input_name INPUT
 * @input_description Input block.
 *
 * @output_name OUTPUT
 * @output_description Permuted block.
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Block length.
 * @parameter_value 1
 *
 * @parameter_name SPREAD
 * @parameter_type int
 * @parameter_description Spread constraint.
 * @parameter_value 1
 *
 * @parameter_name MAX_NRET
 * @parameter_type int
 * @parameter_description Maximum number of retrials in interleaver generation before decreasing S.
 * @parameter_value 100
 *
 * @parameter_name MAX_VRET
 * @parameter_type int
 * @parameter_description Maximum number of retrials for each value before starting a new reetrial.
 * @parameter_value 100
 *
 * @parameter_name INVERSE
 * @parameter_type bool
 * @parameter_description If true, inverse permutation is applied.
 * @parameter_value false
 *
END*/


class SpreadInterleaver : public Interleaver
{
   
	int spread;
	int max_nret;
	int max_vret;

public:
SpreadInterleaver(string nodeName, ParameterSet params)
: Interleaver(nodeName, params)
{
	if(parameters.exist("SPREAD"))
		spread = dereference_cast<int>(parameters.get("SPREAD"));
	else
		spread = 1;

	if(parameters.exist("MAX_NRET"))
		max_nret = dereference_cast<int>(parameters.get("MAX_NRET"));
	else
		max_nret = 100;

	if(parameters.exist("MAX_VRET"))
		max_vret = dereference_cast<int>(parameters.get("MAX_VRET"));
	else
		max_vret = 100;


	Permutation();
}



/** Generate a pseudo-random permutation satisfying given spread constraints
 *   |i - j| + |p[i] - p[j]| > S;
 */
void Permutation()
{
	int i, j;
	int r, x;
	int nret = max_nret;
	int vret = max_vret;
	bool v = false;
	MTRand rng(101);


	while(spread)
	{
		cout << "Spread is " << spread << endl;
		nret = max_nret;
		while(nret--)
		{
			for(i = 0; i < length; i++)
			{
				vret = max_vret;
				v = true;
				while(vret-- && v)
				{
					r = rng.randInt(length - 1 - i);
					x = perm[i + r];
					perm[i + r] = perm[i];
					perm[i] = x;
					v = false;
					for(j = i - 1; j > MAX(i - spread, 0); j--)
					{
						if(ABS(i - j) + ABS(perm[i] - perm[j]) < spread)
						{
							v = true;
							x = perm[i + r];
							perm[i + r] = perm[i];
							perm[i] = x;
							break;
						}
					}
				}
				if(v) break;
			}
		}
		if(!v) break;
		spread--;
	}

}

};

}//namespace FD
