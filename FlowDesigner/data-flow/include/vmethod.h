// Copyright (C) 2001 Jean-Marc Valin
#ifndef _VMETHOD_H_
#define _VMETHOD_H_

#include <string>
#include <map>
#include "Object.h"

namespace FD {
	
	class SymbolSet {
	protected:
		int currentID;
		std::map<std::string,int> translationMap;
	public:
		SymbolSet()
		: currentID(0)
		{}
		int get(const std::string &str)
		{
			std::map<std::string,int>::iterator sym = translationMap.find(str);
			if (sym == translationMap.end())
			{
				translationMap.insert(make_pair(str, currentID++));
				return currentID-1;
			} else {
				return sym->second;
			}
		}
		
		int get (const char *str)
		{
			return get(std::string(str));
		}
		
		std::string reverseLookup (int ID)
		{
			std::map<std::string,int>::iterator it = translationMap.begin();
			while (it!=translationMap.end())
			{
				if (it->second == ID)
					return it->first;
				++it;
			}
			return std::string("");
		}
	};
	
	
	
	class VirtualMethods {
	protected:
		SymbolSet* symbols;
		
		typedef ObjectRef (*funct_ptr0) (ObjectRef x);
		typedef ObjectRef (*funct_ptr1) (ObjectRef x, ObjectRef a);
		typedef ObjectRef (*funct_ptr2) (ObjectRef x, ObjectRef a, ObjectRef b);
		typedef ObjectRef (*funct_ptr3) (ObjectRef x, ObjectRef a, ObjectRef b, ObjectRef c);
		
		typedef std::map<const std::type_info *, funct_ptr0> vtableType0;
		typedef std::map<const std::type_info *, funct_ptr1> vtableType1;
		typedef std::map<const std::type_info *, funct_ptr2> vtableType2;
		typedef std::map<const std::type_info *, funct_ptr3> vtableType3;
		
		std::vector<vtableType0> tables0;
		std::vector<vtableType1> tables1;
		std::vector<vtableType2> tables2;
		std::vector<vtableType3> tables3;
	public:
		VirtualMethods() 
		{
			symbols=new SymbolSet;
		}
		
		int lookup(const std::string &str)
		{
			return symbols->get(str);
		}
		
		int lookup (const char *str)
		{
			return symbols->get(str);
		}
		
		unsigned int registerFunct0(funct_ptr0 ptr, const std::type_info *x, std::string name)
		{
			unsigned int id = symbols->get(name);
			if (id >= tables0.size())
				tables0.resize(id+1);
			tables0[id][x] = ptr;
			return id;
		}
		
		unsigned int registerFunct1(funct_ptr1 ptr, const std::type_info *x, std::string name)
		{
			unsigned int id = symbols->get(name);
			if (id >= tables1.size())
				tables1.resize(id+1);
			tables1[id][x] = ptr;
			return id;
		}
		
		unsigned int registerFunct2(funct_ptr2 ptr, const std::type_info *x, std::string name)
		{
			unsigned int id = symbols->get(name);
			if (id >= tables2.size())
				tables2.resize(id+1);
			tables2[id][x] = ptr;
			return id;
		}
		
		unsigned int registerFunct3(funct_ptr3 ptr, const std::type_info *x, std::string name)
		{
			unsigned int id = symbols->get(name);
			if (id >= tables3.size())
				tables3.resize(id+1);
			tables3[id][x] = ptr;
			return id;
		}
		
		ObjectRef call(int id, ObjectRef x)
		{
			const std::type_info *t1 = &typeid(*x);
			vtableType0 &vtable=tables0[id];
			vtableType0::iterator v1 = vtable.find(t1);
			if (v1!=vtable.end())
			{
				return v1->second(x);
			} else {
				x->doesNotUnderstand(symbols->reverseLookup(id));
				//throw new GeneralException("Virtual function error", __FILE__, __LINE__);
				return nilObject;
			}
		}
		
		ObjectRef call(int id, ObjectRef x, ObjectRef y)
		{
			const std::type_info *t1 = &typeid(*x);
			vtableType1::iterator v1 = tables1[id].find(t1);
			if (v1!=tables1[id].end())
			{
				return v1->second(x,y);
			} else {
				x->doesNotUnderstand(symbols->reverseLookup(id));
				//throw new GeneralException("Virtual function error", __FILE__, __LINE__);
				return nilObject;
			}
		}
		
	};
	
	
	//extern VirtualMethods* vmethod;
	VirtualMethods* vmethod();
	
#define REGISTER_VTABLE0(name, type, func, id) \
static int dummy_vtable_init_for ## _ ## name ## id =\
vmethod()->registerFunct0(func, &typeid(type), # name);
	
}//namespace FD

#endif
