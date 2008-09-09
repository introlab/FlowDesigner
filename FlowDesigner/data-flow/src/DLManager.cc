// Copyright (C) 1999 Jean-Marc Valin

#include "DLManager.h"

using namespace std;

namespace FD {
	
	map<string,LoadedLibrary* > DLManager::loaded;
	map<string, ToolboxData> ToolboxList::loadedToolboxes;
	
	
	LoadedLibrary *DLManager::getLib(const string &name)
	{
		cerr << "DLManager::get_lib(" << name << ")\n";
		if (loaded.find(name)==loaded.end())
		{
			loaded[name] = new LoadedLibrary (name);
		}
		return loaded[name];
	}
	
	
	vector<string> ToolboxList::load(const vector<string> &list, int debug)
	{
		
		vector<string> remain(list);
		vector<string> errors;
		
		
		size_t lastPass;
		do {
			lastPass = remain.size();
			errors.resize(0);
			
			for (size_t i=0;i<remain.size();i++)
			{
				if (debug)
					cerr << "Loading " << remain[i] << "... ";
				DL_HANDLE_TYPE handle = _DL_OPEN(remain[i], debug);
				if (handle)
				{
					if (debug)
						cerr << "[OK]" << endl;
					loadedToolboxes[remain[i]] = ToolboxData(remain[i], handle);
					//FIXME: append toolbox to list.
				} else {
					if (debug)
						cerr << "[Error]" << endl;
					errors.push_back(remain[i]);
				}
			}
			remain = errors;
			if (debug)
				cerr << remain.size() << " errors in pass" << endl;
		} while (remain.size() != 0 && remain.size() != lastPass);
		return errors;
	}
	
}//namespace FD
