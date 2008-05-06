// Copyright (C) 2002 Jean-Marc Valin

#ifndef COMPOSITE_TYPE_H
#define COMPOSITE_TYPE_H

#include "Object.h"
#include "net_types.h"
#include <map>

namespace FD {

/**Allows user-defined types (analog to structs in C/C++)*/
class CompositeType : public Object {
  public:
   typedef std::map<String, ObjectRef> map_type;
   typedef std::pair<String, ObjectRef> map_element_type;
   CompositeType() {}
   void printOn(std::ostream &out) const;
   void readFrom(std::istream &in);
   void serialize(std::ostream &out) const;
   void unserialize(std::istream &in);
   
   void addField(const std::string &name, ObjectRef obj) {fields[name]=obj;}
   void conservativeAddField(const std::string &name, ObjectRef obj);
   ObjectRef get(const std::string &name) const
   {
      map_type::const_iterator it = fields.find(name);
      if (it==fields.end())
         throw new GeneralException(std::string("Unknown field: ") + name, __FILE__, __LINE__);
      return it->second;
   }

   map_type getAllFields()
   {
     return fields;
   }

  private:
   map_type fields;
};

}//namespace FD
#endif
