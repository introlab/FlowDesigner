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

#ifndef _PFPARTICLE_H_
#define _PFPARTICLE_H_

#include "Object.h"
#include <iostream>

namespace RobotFlow {

typedef enum
{
	e_PFP_Generic = 0,
	e_PFP_Unknown
} e_PFP_type;

//
// This should be an abstract base class. Since many containers (like Vector)
// do not allow abstract functions, each abstract function throws an exception
// to avoid the direct use of the base class "abstract" routines.
//
class PFParticle : public FD::Object
{
	friend std::ostream &operator<<(std::ostream &o_out, const PFParticle &i_ref)
	{
		try {
			i_ref.printOn(o_out);
			// Enable cascading
			return o_out;
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception in PFParticle::operator<<:",__FILE__,__LINE__));
		}
	}
	
	friend std::istream &operator>>(std::istream &i_in, PFParticle &o_ref)
	{
		try {
			o_ref.readFrom(i_in);
			// Enable cascading
			return i_in;
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception in PFParticle::operator>>:",__FILE__,__LINE__));
		}
	}
	
public:
	PFParticle()
	: m_pfpType(e_PFP_Unknown)
	{ 
	}
	
	PFParticle(e_PFP_type i_pfpType) 
	: m_pfpType(i_pfpType)
	{ 
	}
	
	PFParticle(const PFParticle &i_ref) 
	{ 
		m_pfpType = i_ref.m_pfpType;
	}

	virtual ~PFParticle()
	{
	
	}

	virtual PFParticle & operator =(const PFParticle &i_ref)
	{
		// Avoid self assignment
		if (&i_ref != this) {
			this->m_pfpType = i_ref.m_pfpType;
		}
		
		return *this;
	}
	
	virtual PFParticle* clone()  const
	{
		return new PFParticle(*this);
	}

	// Default routine to print a PFParticle object to an output stream
	virtual void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in PFParticle::printOn: cannot use base class routine.",__FILE__,__LINE__);
	}

	// Default routine to read a PFParticle object from an input stream
	virtual void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in PFParticle::readFrom: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	e_PFP_type GetType() const 
	{ 
		return m_pfpType; 
	}
	
	void SetType(e_PFP_type i_type)
	{ 
		m_pfpType = i_type; 
	}
	
	virtual unsigned int GetStateSize() const
	{
		throw new FD::GeneralException("Exception in PFParticle::GetStateSize: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual float *GetState()
	{
		throw new FD::GeneralException("Exception in PFParticle::GetState: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual const float *GetCstState() const 
	{
		throw new FD::GeneralException("Exception in PFParticle::GetCstState: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual float GetStateIdx(int i_idx) const
	{
		throw new FD::GeneralException("Exception in PFParticle::GetStateIdx: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual float GetWeight() const
	{
		throw new FD::GeneralException("Exception in PFParticle::GetWeight: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual void SetStateSize(unsigned int i_size) 
	{
		throw new FD::GeneralException("Exception in PFParticle::SetStateSize: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual void SetState(const float *i_state) 
	{
		throw new FD::GeneralException("Exception in PFParticle::SetState: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual void SetStateIdx(int i_idx, float i_val)
	{
		throw new FD::GeneralException("Exception in PFParticle::SetStateIdx: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual void SetWeight(float i_weight) 
	{
		throw new FD::GeneralException("Exception in PFParticle::SetWeight: cannot use base class routine.",__FILE__,__LINE__);
	}

private:
	e_PFP_type m_pfpType;
};

}

#endif
