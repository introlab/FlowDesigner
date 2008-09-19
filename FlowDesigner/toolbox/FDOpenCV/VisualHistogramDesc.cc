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

#include "VisualHistogramDesc.h"

using namespace FD;
using namespace std;

namespace RobotFlow {

//
// Declaring known types
//
static int VISUALHISTDESC_VAR_0 = Object::addObjectType<VisualHistogramDesc<double, unsigned char> >
	("<VisualHistogramDesc<double,unsigned char>>", new ObjectFactory<VisualHistogramDesc<double, unsigned char> >("<VisualHistogramDesc<double,unsigned char>>"));
static int VISUALHISTDESC_VAR_1 = Object::addObjectType<VisualHistogramDesc<double, unsigned int> >
	("<VisualHistogramDesc<double,unsigned int>>", new ObjectFactory<VisualHistogramDesc<double, unsigned int> >("<VisualHistogramDesc<double,unsigned int>>"));

template<>
const double VisualHistogramDesc<double, unsigned char>::k_VISUALHIST_2DIVPI = 2.0/3.14159265358979323846;

template<>
const double VisualHistogramDesc<double, unsigned int>::k_VISUALHIST_2DIVPI = 2.0/3.14159265358979323846;
}//namespace RobotFlow
