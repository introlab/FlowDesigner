// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <iostream>
#include <stdlib.h>
#include "flow_pref.h"
#include <fstream>
#include <sstream>

FlowPref FlowPref::pref;

FlowPref::FlowPref()
{
   params["ShowAllInOut"] = "no";
   params["ShowTooltips"] = "yes";
   params["PrintOutput"]  = "yes";
   params["RunProcess"]   = "no";
   params["RegularColor"] = "0x8cd0afff";
   //params["SelectedColor"]= "0x8ca0af20";
   //params["SelectedColor"]= "0x8087c020";
   params["SelectedColor"]= "0xa8b2fcff";
   //params["ErrorColor"]   = "0xc02020ff";
   params["ErrorColor"]   = "0xfc9595ff";

   string filename = getenv("HOME");
   filename += "/.vflowrc";
   ifstream prefFile(filename.c_str());
   if (prefFile.fail())
   {
      save();
      modified=false;
      return;
   }
   while (1)
   {
      string key;
      string value;
      char ch;
      while(1)
      {
	 prefFile >> ch;
	 if (!prefFile)
	    return;
	 if (ch=='=')
	    break;
	 if (ch!=' ')
	    key += ch;
      }
      prefFile >> value;
      params[key]   = value;
      //cerr << key << "->" << value << endl;
   }
   modified=false;
}

FlowPref::~FlowPref()
{
   //if (modified)
   //   save();
}

bool FlowPref::getBool(const string &str)
{
   string val = pref.params[str];
   //cerr << "str = " << val << endl;
   if (val=="yes" || val=="YES" || val=="true" || val=="TRUE")
      return true;
   else
      return false;
}

void FlowPref::setBool(const string &str, bool val)
{
   if (val)
      pref.params[str] = "yes";
   else
      pref.params[str] = "no";
   pref.modified=true;
}

unsigned int FlowPref::getColor(const string &str)
{
   string val = pref.params[str];
   istringstream valStr(val);
   char ch1, ch2;
   unsigned int col=0;
   valStr >> ch1 >> ch2;
   if (ch1 != '0' || ch2 != 'x')
   {
      cerr << "invalid color\n";
      return col;
   }
   for (int i=0;i<8;i++)
   {
      col <<= 4;
      valStr >> ch1;
      if (ch1 >= '0' && ch1 <= '9')
	 col += (ch1-'0');
      else if (ch1 >= 'a' && ch1 <= 'f')
	 col += 10+(ch1-'a');
      else
	 cerr << "invalid color char: " << ch1 << endl;
   }
   return col;
}

void FlowPref::setColor(const string &str, unsigned int col)
{
   char colStr[11];
   colStr[10]=0;
   colStr[0]='0';
   colStr[1]='x';
   for (int i=0;i<8;i++)
   {
      int dhex = col&15;
      if (dhex<=9)
	 colStr[9-i]=dhex+'0';
      else
	 colStr[9-i]=dhex-10+'a';
      col>>=4;
   }
   pref.params[str]=colStr;
}


void FlowPref::Save()
{
   pref.save();
}

void FlowPref::save()
{
   string filename = getenv("HOME");
   filename += "/.vflowrc";
   //cerr << "save " << filename << endl;
   ofstream prefFile(filename.c_str());
   if (prefFile.fail())
      return;
   map<string,string>::iterator p = params.begin();
   while (p != params.end())
   {
      prefFile << p->first << "=" << p->second << endl;
      p++;
   }
   modified=false;
   //cerr << "pref save\n";
}

