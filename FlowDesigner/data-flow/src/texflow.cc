// Copyright (C) 2001 Jean-Marc Valin

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"

using namespace std;

namespace FD {

void node2html(string nodeName, NodeInfo *info, ostream &out)
{
   int nb;


   out << "\\textbf{" << nodeName << "} (" << info->category << ")\n";
   if (info->requireList.size())
      out << "(require: " << info->requireList << ")";
   out << info->description << endl << endl;



   out << "\\begin{tabular}[h]{|l|l|l|p{5cm}|}\n\\hline & Name & Type & Description \\\\\n";
   out << "\\hline \\hline\n";
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
      out << fieldName << " & ";
      vector<ItemInfo *> &fieldInfo= *fieldInfoPtr;
      nb = fieldInfo.size();
      if (nb > 0)
      {
	 for (int i=0;i<nb;i++)
	 {
	    if (i>0)
	       out << "& ";
	    out << fieldInfo[i]->name << " & " << fieldInfo[i]->type << " & " << fieldInfo[i]->description << "\\\\\n";
	    if (i!=nb-1)
	       out << "\\cline{2-4}\n";
	 }
      } else {
	 out << " none & &\\\\";
      }
      out << "\\hline\n";
   }
   
   out << "\\end{tabular}\n\n";
   out << "\\vspace{0,3cm}\n";
}

int main(int argc, char **argv)
{
   //UIDocument::loadAllInfo();
   UINodeRepository::Scan();
   
   ostream &out = cout;

   
   UINodeRepository::iterator i;

   int count=0;
   /*i = UINodeRepository::Begin();
   while (i != UINodeRepository::End())
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


   out << "\n<h1>Nodes Documentation</h1>\n\n";*/
   i = UINodeRepository::Begin();
   while (i != UINodeRepository::End())
   {
      node2html(i->first, i->second, out);
      i++;
   }


}
}//namespace FD
