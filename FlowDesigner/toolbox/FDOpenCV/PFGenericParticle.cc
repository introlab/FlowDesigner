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

#include "PFGenericParticle.h"

using namespace std;
using namespace FD;

namespace RobotFlow {

DECLARE_TYPE(PFGenericParticle)

PFGenericParticle::PFGenericParticle()
: PFParticle(e_PFP_Generic),
m_stateSize(0),
m_state(NULL),
m_weight(-1)
{ 
}

PFGenericParticle::PFGenericParticle(unsigned int i_stateSize) 
: PFParticle(e_PFP_Generic),
m_stateSize(i_stateSize),
m_state(NULL),
m_weight(-1)
{ 
	m_state = new float [m_stateSize];
}

PFGenericParticle::PFGenericParticle(const PFGenericParticle &i_ref) 
{ 
	this->SetType(i_ref.GetType());
	m_stateSize = i_ref.m_stateSize;
	m_state = new float [m_stateSize];
	m_weight = i_ref.m_weight;
	
	for (int i=0; i<m_stateSize; i++) {
		m_state[i] = i_ref.m_state[i];
	}
}

PFGenericParticle::~PFGenericParticle()
{
	delete [] m_state;
}

PFGenericParticle & PFGenericParticle::operator =(const PFGenericParticle &i_ref)
{
	// Avoid self assignment
	if (&i_ref != this) {
		this->SetType(i_ref.GetType());
		
		if (m_stateSize != i_ref.m_stateSize) {
			m_stateSize = i_ref.m_stateSize;
			delete m_state;
			m_state = new float [m_stateSize];
		}
		
		if (m_state && i_ref.m_state) {
			for (int i=0; i<m_stateSize; i++) {
				m_state[i] = i_ref.m_state[i];
			}
		}
		
		m_weight = i_ref.m_weight;
	}
	
	return *this;
}

PFGenericParticle* PFGenericParticle::clone()  const
{
	return new PFGenericParticle(*this);
}

// Default routine to print a PFGenericParticle object to an output stream
void PFGenericParticle::printOn(ostream &out) const
{
	throw new GeneralException("Exception in PFGenericParticle::printOn: routine not yet implemented.",__FILE__,__LINE__);
}

// Default routine to read a PFGenericParticle object from an input stream
void PFGenericParticle::readFrom(istream &in)
{
	throw new GeneralException("Exception in PFGenericParticle::readFrom: routine not yet implemented.",__FILE__,__LINE__);
}

unsigned int PFGenericParticle::GetStateSize() const
{
	return m_stateSize;
}

float *PFGenericParticle::GetState()
{
	return m_state;
}

const float *PFGenericParticle::GetCstState() const
{
	return (const float *) m_state;
}

float PFGenericParticle::GetStateIdx(int i_idx) const
{
	if (i_idx < 0 || i_idx >= m_stateSize) {
		throw new GeneralException("Exception in PFGenericParticle::GetStateIdx: invalid state index.",__FILE__,__LINE__);
	}
	
	return m_state[i_idx];
}

float PFGenericParticle::GetWeight() const
{
	return m_weight;
}

void PFGenericParticle::SetStateSize(unsigned int i_size)
{
	if (i_size != m_stateSize) {
		m_stateSize = i_size;
		
		delete [] m_state;
		m_state = new float[m_stateSize];
	}
}

// Assumes that given ptr has the same size as current particle
void PFGenericParticle::SetState(const float *i_state)
{
	for (int i=0; i<m_stateSize; i++) {
		m_state[i] = i_state[i];
	}
}
	
void PFGenericParticle::SetStateIdx(int i_idx, float i_val)
{
	if (i_idx < 0 || i_idx >= m_stateSize) {
		throw new GeneralException("Exception in PFGenericParticle::SetStateIdx: invalid state index.",__FILE__,__LINE__);
	}
	
	m_state[i_idx] = i_val;
}
	
void PFGenericParticle::SetWeight(float i_weight)
{
	m_weight = i_weight;
}

}

