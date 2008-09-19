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

#ifndef _VISUALFEATUREDESC_H_
#define _VISUALFEATUREDESC_H_

#include "Object.h"
#include <iostream>

namespace RobotFlow {

typedef enum
{
	e_VISUALDESCRIPTOR_histogram = 0,
	e_VISUALDESCRIPTOR_integral,
	e_VISUALDESCRIPTOR_unknown
} e_VISUALDESCRIPTOR_type;

//
// This should be an abstract base class. Since many containers (like Vector)
// do not allow abstract functions, each abstract function throws an exception
// to avoid the direct use of the base class "abstract" routines.
//
template <class FeatBaseType>
class VisualFeatureDesc : public FD::Object
{
	friend std::ostream &operator<<(std::ostream &o_out, const VisualFeatureDesc<FeatBaseType> &i_ref)
	{
		try {
			i_ref.printOn(o_out);
			// Enable cascading
			return o_out;
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception in VisualFeatureDesc::operator<<:",__FILE__,__LINE__));
		}
	}
	
	friend std::istream &operator>>(std::istream &i_in, VisualFeatureDesc<FeatBaseType> &o_ref)
	{
		try {
			o_ref.readFrom(i_in);
			// Enable cascading
			return i_in;
		}
		catch (FD::BaseException *e) {
			throw e->add(new FD::GeneralException("Exception in VisualFeatureDesc::operator>>:",__FILE__,__LINE__));
		}
	}
	
public:
	VisualFeatureDesc()
	: m_descType(e_VISUALDESCRIPTOR_unknown)
	{ 
	}
	
	VisualFeatureDesc(e_VISUALDESCRIPTOR_type i_descType) 
	: m_descType(i_descType)
	{ 
	}
	
	VisualFeatureDesc(const VisualFeatureDesc<FeatBaseType> &i_ref) 
	{ 
		m_descType = i_ref.m_descType;
	}

	virtual ~VisualFeatureDesc()
	{
	
	}

	virtual VisualFeatureDesc<FeatBaseType> & operator =(const VisualFeatureDesc<FeatBaseType> &i_ref)
	{
		// Avoid self assignment
		if (&i_ref != this) {
			this->m_descType = i_ref.m_descType;
		}
		
		return *this;
	}
	
	virtual VisualFeatureDesc<FeatBaseType>* clone()  const
	{
		return new VisualFeatureDesc<FeatBaseType>(*this);
	}

	// Default routine to print a VisualFeatureDesc object to an output stream
	virtual void printOn(std::ostream &out) const
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::printOn: cannot use base class routine.",__FILE__,__LINE__);
	}

	// Default routine to read a VisualFeatureDesc object from an input stream
	virtual void readFrom(std::istream &in)
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::readFrom: cannot use base class routine.",__FILE__,__LINE__);
	}

	virtual double Similarity(const FeatBaseType *i_candidate, unsigned int i_size) const
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::Similarity: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual void Adapt(const FeatBaseType *i_candidate, unsigned int i_size, double i_rate)
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::Adapt: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual unsigned int GetSize() const
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::GetSize: cannot use base class routine.",__FILE__,__LINE__);
	}

	virtual FeatBaseType *GetFeatures()
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::GetFeatures: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual const FeatBaseType *GetCstFeatures() const
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::GetCstFeatures: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual bool GetValidity() const
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::GetValidity: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	e_VISUALDESCRIPTOR_type GetType() const { return m_descType; }
	
	void SetType(e_VISUALDESCRIPTOR_type i_type)
	{ 
		m_descType = i_type; 
	}
	
	virtual void SetSize(unsigned int i_size)
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::SetSize: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual void SetFeatures(const FeatBaseType *i_ref, unsigned int i_size)
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::SetFeatures: cannot use base class routine.",__FILE__,__LINE__);
	}
	
	virtual void SetValidity(bool i_flag)
	{
		throw new FD::GeneralException("Exception in VisualFeatureDesc::SetValidity: cannot use base class routine.",__FILE__,__LINE__);
	}
	
private:
	e_VISUALDESCRIPTOR_type m_descType;
};

}

#endif
