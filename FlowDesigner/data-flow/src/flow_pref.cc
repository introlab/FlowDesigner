// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <iostream>
#include <stdlib.h>
#include "flow_pref.h"
#include <fstream>
#include <sstream>
#include <libxml/tree.h>
#include <libxml/parser.h>

FlowPref FlowPref::pref;

FlowPref::FlowPref()
{
   params["VFLOW"]["ShowAllInOut"] = "no";
   params["VFLOW"]["ShowTooltips"] = "yes";
   params["VFLOW"]["PrintOutput"]  = "yes";
   params["VFLOW"]["RunProcess"]   = "no";
   params["VFLOW"]["RegularColor"] = "0x8cd0af80";
   params["VFLOW"]["SelectedColor"]= "0xa8b2fc80";
   params["VFLOW"]["ErrorColor"]   = "0xfc959580";
   
   string filename = getenv("HOME");
   filename += "/.flowrc";

   xmlDocPtr doc = xmlParseFile (filename.c_str());
   if (!doc || !doc->children || !doc->children->name)
   {
      cerr << "No (valid) preference file found, one will be created in ~/.flowrc" << endl;
      modified=true;
      return;
   }
   xmlNodePtr root=doc->children;
   xmlNodePtr cat = root->children;
   while(cat)
   {
      if (string((char*)cat->name) == "Category")
      {
	 xmlNodePtr par=cat->children;
	 char *str_catname = (char *) xmlGetProp(cat, (xmlChar *)"name");
	 if (str_catname)
	 {
	    while (par)
	    {
	       if (string((char*)par->name) == "Parameter")
	       {
		  char *str_name = (char *) xmlGetProp(par, (xmlChar *)"name");
		  char *str_value = (char *) xmlGetProp(par, (xmlChar *)"value");
		  if (str_name && str_value)
		  {
		     params[str_catname][str_name] = str_value;
		     free(str_name); free(str_value);
		  }
	       }
	       par=par->next;
	    }
	    free(str_catname);
	 }
      }
      cat=cat->next;
   }
   
   modified=false;
}

FlowPref::~FlowPref()
{
   if (modified)
      save();
}

bool FlowPref::getBool(const string &cat, const string &str)
{
   string val = pref.params[cat][str];
   //cerr << "str = " << val << endl;
   if (val=="yes" || val=="YES" || val=="true" || val=="TRUE")
      return true;
   else
      return false;
}

void FlowPref::setBool(const string &cat, const string &str, bool val)
{
   if (val)
      pref.params[cat][str] = "yes";
   else
      pref.params[cat][str] = "no";
   pref.modified=true;
}

unsigned int FlowPref::getColor(const string &cat, const string &str)
{
   string val = pref.params[cat][str];
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

void FlowPref::setColor(const string &cat, const string &str, unsigned int col)
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
   pref.params[cat][str]=colStr;
}


void FlowPref::Save()
{
   pref.save();
}

void FlowPref::save()
{
   string filename = getenv("HOME");
   filename += "/.flowrc";
   xmlDocPtr doc;
   doc = xmlNewDoc((xmlChar *)"1.0");
   doc->children = xmlNewDocNode(doc, NULL, (xmlChar *)"Preferences", NULL);
   xmlSetProp(doc->children, (xmlChar *)"version", (xmlChar *)OVERFLOW_VERSION);

   map<string, map<string,string> >::iterator cat = params.begin();
   while (cat != params.end())
   {
      xmlNodePtr catNode = xmlNewChild(doc->children, NULL, (xmlChar *)"Category", NULL);
	 xmlSetProp(catNode, (xmlChar *)"name", (xmlChar *)cat->first.c_str());
	 map<string,string>::iterator it = cat->second.begin();
      while (it!=cat->second.end())
      {
	 xmlNodePtr paramNode = xmlNewChild(catNode, NULL, (xmlChar *)"Parameter", NULL);
	 xmlSetProp(paramNode, (xmlChar *)"name", (xmlChar *)it->first.c_str());
	 xmlSetProp(paramNode, (xmlChar *)"value", (xmlChar *)it->second.c_str());	 
	 it++;
      }
      cat++;
   }
   
   xmlSaveFile(filename.c_str(), doc);
   
   xmlFreeDoc(doc);


   modified=false;
}

