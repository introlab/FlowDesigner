#include <string>
#include <map>
#include "Object.h"

class SymbolSet {
  protected:
   int currentID;
   map<string,int> translationMap;
  public:
   SymbolSet()
      : currentID(0)
   {}
   int get(string str)
   {
      map<string,int>::iterator sym = translationMap.find(str);
      if (sym == translationMap.end())
      {
	 translationMap.insert(make_pair(str, currentID++));
      } else {
	 return sym->second;
      }
   }
   
};

extern SymbolSet* symbols;
