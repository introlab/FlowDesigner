#ifndef TYPE_MAP_H
#define TYPE_MAP_H

#include <algorithm>
#include <map>
#include <typeinfo>

struct compare_const_type_info_ptr : public binary_function<const type_info *, const type_info *, bool>
{
   bool operator()(const type_info *lhs, const type_info *rhs) const
   {
      return lhs->before(*rhs);
   }
};

template<typename T>
class TypeMap : public map<const type_info *, T, compare_const_type_info_ptr >
{
};


#endif
