// Copyright (C) 2001 Jean-Marc Valin

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"

void node2html(string nodeName, SubnetInfo *info, ostream &out)
{
   int nb;

   out << "<p><a NAME=\"" << nodeName << "\"></a>\n"
       << "<h3>\n"
       << nodeName << " (" << info->category << ")</h3>\n"
       << info->description
       << "<br>&nbsp;\n"
       << "<table BORDER WIDTH=\"100%\" NOSAVE >\n"
       << "<tr>\n"
       << "<td></td>\n"
       << "<td>NAME</td>\n"
       << "<td>TYPE</td>\n"
       << "<td>MEANING</td>\n"
       << "</tr>\n";

   for (int field = 0;field<3;field++)
   {
      string fieldName;
      vector<ItemInfo *> *fieldInfoPtr;
      switch (field) {
	 case 0:
	    fieldName="Inputs";
	    fieldInfoPtr = &info->inputs;
	    break;
	 case 1:
	    fieldName="Outputs";
	    fieldInfoPtr = &info->outputs;
	    break;
	 case 2:
	    fieldName="Parameters";
	    fieldInfoPtr = &info->params;
	    break;
      }
      vector<ItemInfo *> &fieldInfo= *fieldInfoPtr;
      out << "<tr NOSAVE>\n"
	  << "<td>" << fieldName << "</td>\n";
      nb = fieldInfo.size();
      if (nb > 0)
      {
	 out << "<td>";
	 for (int i=0;i<nb;i++)
	 {
	    if (i>0)
	       out << "<br>";
	    out << fieldInfo[i]->name;
	 }
	 out << "</td>";
	 
	 out << "<td>";
	 for (int i=0;i<nb;i++)
	 {
	    if (i>0)
	       out << "<br>";
	    out << fieldInfo[i]->type;
	 }
	 out << "</td>";
	 
	 out << "<td>";
	 for (int i=0;i<nb;i++)
	 {
	    if (i>0)
	       out << "<br>";
	    out << fieldInfo[i]->description;
	 }
	 out << "</td>";
      } else {
	 out << "<td>none</td>";
      }
      out << "</tr>\n";
   }
   
   out << "</table>\n";
}

int main(int argc, char **argv)
{
   UIDocument::loadAllInfo();

   ostream &out = cout;

   out << "<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">\n"
       << "<html>\n"
       << "<head>\n"
       << "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n"
       << "<meta name=\"GENERATOR\" content=\"Overflow\">\n"
       << "<meta name=\"Author\" content=\"Jean-Marc Valin\">\n"
       << "<title>Overflow node documentation</title>\n"
       << "<!-- This page was created by the Overflow documentation generator -->\n"
       << "</head>\n"
       << "<body>\n";
   
   map<string, SubnetInfo *>::iterator i;

   out << "<h1>List of available Overflow Nodes</h1>\n\n";
   out << "<center><table BORDER COLS=3 WIDTH=\"100%\" NOSAVE >\n\n";
   int count=0;
   i = UIDocument::externalDocInfo.begin();
   while (i != UIDocument::externalDocInfo.end())
   {
      if (count %3==0)
	 out << "<tr>\n";
      out << "<td><a href=\"#" << i->first << "\">" << i->first << "</a></td>\n";
      if (count %3==2)
	 out << "</tr>\n";
      count++;
      i++;
   }   
   if (count %3!=0)
      out << "</tr>\n";
   out << "</table></center>\n";


   out << "\n<h1>Nodes Documentation</h1>\n\n";
   i = UIDocument::externalDocInfo.begin();
   while (i != UIDocument::externalDocInfo.end())
   {
      node2html(i->first, i->second, out);
      i++;
   }
   out << "\n</body>\n"
       << "</html>\n";


}
