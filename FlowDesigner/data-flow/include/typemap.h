// Copyright (C) 2001 Jean-Marc Valin
#ifndef TYPE_MAP_H
#define TYPE_MAP_H

#include <algorithm>
#include <map>
#include <typeinfo>


struct compare_const_type_info_ptr : public std::binary_function<const std::type_info *, const std::type_info *, bool>
{
   bool operator()(const std::type_info *lhs, const std::type_info *rhs) const
   {
      return lhs->before(*rhs);
   }
};

template<typename T>
class TypeMap : public std::map<const std::type_info *, T, compare_const_type_info_ptr >
{
};


#endif
