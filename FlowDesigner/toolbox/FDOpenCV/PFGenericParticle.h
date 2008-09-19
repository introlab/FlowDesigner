/* Copyright (C) 2005 Pierre Moisan (Pierre.Moisan@USherbrooke.ca) 

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef _PFGENERICPARTICLE_H_
#define _PFGENERICPARTICLE_H_

#include "PFParticle.h"
#include <iostream>

namespace RobotFlow {

class PFGenericParticle : public PFParticle
{
public:
	PFGenericParticle();
	
	PFGenericParticle(unsigned int i_stateSize);
	
	PFGenericParticle(const PFGenericParticle &i_ref);

	virtual ~PFGenericParticle();

	virtual PFGenericParticle & operator =(const PFGenericParticle &i_ref);
	
	virtual PFGenericParticle* clone()  const;

	// Default routine to print a PFGenericParticle object to an output stream
	virtual void printOn(std::ostream &out) const;

	// Default routine to read a PFGenericParticle object from an input stream
	virtual void readFrom(std::istream &in);
	
	virtual unsigned int GetStateSize() const;
	
	virtual float *GetState();
	
	virtual float GetStateIdx(int i_idx) const;
	
	virtual const float *GetCstState() const;
	
	virtual float GetWeight() const;
	
	virtual void SetStateSize(unsigned int i_size);
	
	virtual void SetState(const float *i_state);
	
	virtual void SetStateIdx(int i_idx, float i_val);
	
	virtual void SetWeight(float i_weight);

private:
	int m_stateSize;
	float *m_state;
	float m_weight;
};

}

#endif
