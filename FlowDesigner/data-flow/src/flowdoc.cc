// Copyright (C) 2001 Jean-Marc Valin

#include "UIDocument.h"
#include <string>
#include "ParameterSet.h"
#include "ObjectRef.h"
#include <vector>

void node2html(string nodeName, NodeInfo *info, ostream &out)
{
   int nb;

   out << "<p><a NAME=\"" << nodeName << "\"></a>\n"
       << "<h3>\n"
       << nodeName << " (" << info->category << ")</h3>\n";
   if (info->requireList.size())
      out << "<i>(require: " << info->requireList << ")</i><br>";
   out << info->description;

   if(nodeName == "Constant")
     out << "<br><br>For further explainations see Users Manual at section 6.3 \"Data Types\": "
	 << "<a href=\"http://freespeech.sourceforge.net/doc/user-manual/node8.html#SECTION00830000000000000000\">Users Manual</a>";


   out << "<br>&nbsp;\n"
       << "<table BORDER COLS=4 WIDTH=\"75%\" NOSAVE >\n"
       << "<tr>\n"
       << "<td WIDTH=\"10%\"></td>\n"
       << "<th WIDTH=\"15%\">NAME</th>\n"
       << "<th WIDTH=\"10%\">TYPE</th>\n"
       << "<th WIDTH=\"40%\">MEANING</th>\n"
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

      nb = fieldInfo.size();
      if (nb == 1)
      {

	out << "<tr NOSAVE>\n"
	    << "<th WIDTH=\"10%\">" << fieldName << "</th>\n";

	out << "<td WIDTH=\"15%\">";
	out << fieldInfo[0]->name;
	out << "</td>";
  	 
	out << "<td WIDTH=\"10%\">";
	out << fieldInfo[0]->type;
	out << "</td>";
  	 
	out << "<td WIDTH=\"40%\">";
	out << fieldInfo[0]->description;
	out << "</td>";
      }
      else if ( nb > 1 )
	{

	  for (int i=0;i<nb;i++)
	    {
	      if (i == 0)
		{
		  out << "<tr NOSAVE>\n"
		      << "<th WIDTH=\"10%\" ROWSPAN=\"#" << nb << "\">" << fieldName << "</th>\n";
		}
	      
	      out << "<td WIDTH=\"15%\">";
	      out << fieldInfo[i]->name;
	      out << "</td>";
  	 
	      out << "<td WIDTH=\"15%\">";
	      out << fieldInfo[i]->type;
	      out << "</td>";
  	 
	      out << "<td WIDTH=\"40%\">";
	      out << fieldInfo[i]->description;
	      out << "</td></tr>";
	    }
	}
      else {
	out << "<tr NOSAVE>\n"
	    << "<th WIDTH=\"10%\">" << fieldName << "</th>\n";
  	out << "<td>none</td>";
  	out << "<td>none</td>";
  	out << "<td>none</td>";
      }
      out << "</tr>\n";
   }
   
   out << "</table>\n";

}











void categContent( string categName, string nextcateg, ostream &out, UINodeRepository::iterator i )
{

// Initialisation des variables

  vector<string> listnodes(200);

  int j = 0;
  int nbnodes = 0;


// On cree une liste des noeuds de la categorie

   i = UINodeRepository::Begin();


   while (i != UINodeRepository::End())
   {
     if(i->second->category == categName)
       {
	 listnodes[j] = i->first;
	 nbnodes++;
	 j++;
       }
     i++;
   }


  out << "<p><a NAME=\"" << categName << "\"></a>"
      << "<h2>" << "<br><br><br><br><br><br><hr><br>* "  << categName << " (" << nbnodes << ")"
      <<"</h2>"  
      << "<h3>List of available Overflow Nodes "<< "</h3>\n\n";


// Affichage du tableau : noeuds de la categorie

   out << "<table BORDER COLS=3 WIDTH=\"60% \" NOSAVE >\n\n";
   int nbcol = 3;

   switch (nbnodes % nbcol)
     {

     case 0:
       for(int k=0; k < (nbnodes / nbcol); k++)
	 {
	   out << "<tr><td>" <<"* " <<"<a href=\"#" << listnodes[k] << "\">" << listnodes[k] <<"</a></td>\n";
	   
	   if(listnodes[(nbnodes / nbcol ) + k ] != "")
	     {
	       out   << "<td>" <<"* " <<"<a href=\"#" << listnodes[(nbnodes / nbcol ) + k ] << "\">" << listnodes[(nbnodes / nbcol ) + k ] <<"</a></td>\n";
	     }
	   else
	     out << "</tr>\n";

	   if(listnodes[(2 * nbnodes / nbcol ) + k] != "")
	     {
	       out << "<td>" <<"* " <<"<a href=\"#" << listnodes[(2 * nbnodes / nbcol ) + k] << "\">" << listnodes[(2 * nbnodes / nbcol ) + k] <<"</a></td></tr>\n";
	     }
	   else
	     out << "</tr>\n";
	 }   
       break;

     case 1:

       for(int k=0; k <= (nbnodes / nbcol); k++)
	 {
	   out << "<tr><td>" <<"* " <<"<a href=\"#" << listnodes[k] << "\">" << listnodes[k] <<"</a></td>\n";
	   
	   if(listnodes[(nbnodes / nbcol) + k + 1] != "")
	     {
	       out   << "<td>" <<"* " <<"<a href=\"#" << listnodes[(nbnodes / nbcol ) + k + 1] << "\">" << listnodes[(nbnodes / nbcol) + k + 1] <<"</a></td>\n";
	     }
	   else
	     out<< "</tr>\n";

	   if(listnodes[(2 * nbnodes / nbcol) + k + 2] != "")
	     {
	       out << "<td>" <<"* " <<"<a href=\"#" << listnodes[(2 * nbnodes / nbcol ) + k + 2] << "\">" << listnodes[(2 * nbnodes / nbcol) + k + 2] <<"</a></td></tr>\n";
	     }
	   else
	     out << "</tr>\n";

	 }
       break;

     case 2:
       for(int k=0; k <= (nbnodes / nbcol); k++)
	 {
	   out << "<tr><td>" <<"* " <<"<a href=\"#" << listnodes[k] << "\">" << listnodes[k] <<"</a></td>\n";
	   
	   if(listnodes[(nbnodes / nbcol) + k + 1] != "")
	     {
	       out   << "<td>" <<"* " <<"<a href=\"#" << listnodes[(nbnodes / nbcol ) + k + 1] << "\">" << listnodes[(nbnodes / nbcol) + k + 1] <<"</a></td>\n";
	     }
	   else
	     out << "</tr>\n";

	   if( listnodes[(2 * nbnodes / nbcol) + k + 1] != "")
	     {
	       out << "<td>" <<"* " <<"<a href=\"#" << listnodes[(2 * nbnodes / nbcol ) + k + 1] << "\">" << listnodes[(2 * nbnodes / nbcol) + k + 1] <<"</a></td></tr>\n";
	     }
	   else
	     out << "</tr>\n";
	 }
       break;

     default:
       out << "\nProblems occured while getting the data\n";
     }

   out << "</table>\n ";

   if(nextcateg != "")
     {
       out << "<br>See next category: "
	   << "<a href=\"#" << nextcateg << "\">"
	   << nextcateg << "</a><br>";
     }
   out << "<br>Return to: "
       << "<a href=\"#" << "Categories of available Overflow Nodes" << "\">"
       << "Categories of available Overflow Nodes</a><br><br><hr><br><br>";


// Affichage des informations sur les noeuds


   for(int l=0; l < nbnodes; l++)
     {
       i = UINodeRepository::Begin();

       while (i != UINodeRepository::End())
	 {
	   if(i->first == listnodes[l])
	     {
	       out <<"<br>";
	       node2html(listnodes[l], i->second, out);

	       out << "<br>Return to: "
		   << "<a href=\"#" << categName << "\">" 
		   << categName <<"</a><br>";

	       if(nextcateg != "")
		 {
		   out << "<br>See next category: "
		       << "<a href=\"#" << nextcateg << "\">"
		       << nextcateg << "</a><br>";
		 }
	       out << "<br>Return to: "
		   << "<a href=\"#" << "Categories of available Overflow Nodes" << "\">"
		   << "Categories of available Overflow Nodes</a><br>";
	     }
	   i++;
	 }
     }
}















int main(int argc, char **argv)
{


// Initialisation des variables

   UINodeRepository::Scan();
   
   int nbcateg = 0;
   vector<string> listcateg(100);
   int newcateg = 1;

   int nbnodes = 0;

   string stock = "";

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
       << "<body>";
   
   UINodeRepository::iterator i;


// On receuille les categories differentes dans une liste

   i = UINodeRepository::Begin();
   listcateg[0]= i->second->category;
   i++;
   nbcateg++;
   nbnodes++;


   while (i != UINodeRepository::End())
   {
     newcateg = 1;
     int count = 0;


     for(int j = 0; j < nbcateg ; j++)
       if( listcateg[j] == i->second->category )
	 newcateg = 0;


     if(newcateg == 1)
       {
	 listcateg[nbcateg] = i->second->category;
	 nbcateg++;
       }

     nbnodes++;
     i++;
   }   



// Tri des categories en ordre alphabetique dans la liste

   for (int passage=0; passage < (nbcateg - 1) ; passage++)
       for (int k=0; k < (nbcateg - 1); k++)
	   if (listcateg[k] > listcateg[k +1])
	     {
	       stock = listcateg[k];
	       listcateg[k] = listcateg[k + 1];
	       listcateg[k + 1] = stock;
	     }


// Affichage du tableau : categories de noeuds

   out << "<a NAME=\"" << "Categories of available Overflow Nodes" << "\"></a>"; 

   out << "<p><br><center><h1><b>" << "* NODE" <<"&nbsp " <<" DOCUMENTATION *</b><br><br>";
   out << "Categories of available Overflow Nodes"
       << "</h1>\n<br>";
   out << "<table BORDER COLS=2 WIDTH=\"40% \" NOSAVE >\n\n";

   int nbcol = 2;

   for(int j = 0; j <= ( nbcateg / nbcol) ; j++)
    {
       out << "<tr><td>" <<"* " <<"<a href=\"#" << listcateg[j] << "\">" << listcateg[j] <<"</a></td>\n"
	     << "<td>" <<"* " <<"<a href=\"#" << listcateg[(nbcateg / nbcol ) + j + 1] << "\">" << listcateg[(nbcateg / nbcol) + j + 1] <<"</a></td></tr>\n";
     }


   out << "</table></center>\n";
   out << "<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"
       << "<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>"
       << "<hr><br>"
       << "<h1>Categories:</h1>";


   for(int a=0; a < nbcateg; a++)
     categContent( listcateg[a], listcateg[a + 1], out, i);

   out << "\n</body>\n"
       << "</frameset>"
       << "</html>\n";
}

// ANCIENNE VERSION DU PROGRAMME (PARTIE MAIN)


//int main(int argc, char **argv)
//{
//     UIDocument::loadAllInfo();
//   UINodeRepository::Scan();
   
//   ostream &out = cout;

//   out << "<!doctype html public \"-//w3c//dtd html 4.0 transitional//en\">\n"
//       << "<html>\n"
//       << "<head>\n"
//       << "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n"
//       << "<meta name=\"GENERATOR\" content=\"Overflow\">\n"
//       << "<meta name=\"Author\" content=\"Jean-Marc Valin\">\n"
//       << "<title>Overflow node documentation</title>\n"
//       << "<!-- This page was created by the Overflow documentation generator -->\n"
//       << "</head>\n"
//       << "<body>\n";
   
//   UINodeRepository::iterator i;

//Parcours la banque de donnees et affiche le grand tableau


//   out << "<h1>List of available Overflow Nodes Category</h1>\n\n";
//   out << "<center><table BORDER COLS=3 WIDTH=\"100%\" NOSAVE >\n\n";
//   int count=0;
//   i = UINodeRepository::Begin();
//   while (i != UINodeRepository::End())
//   {
//      if (count %3==0)
//	 out << "<tr>\n";
//      out << "<td><a href=\"#" << i->first << " (" << i->second->category << ")" << "\">" << i->first << " (" << i->second->category << ")" << "</a></td>\n";
//      if (count %3==2)
//	 out << "</tr>\n";
//      count++;
//      i++;
//   }   
//   if (count %3!=0)
//      out << "</tr>\n";
//   out << "</table></center>\n";


//Affiche les informations sur les noeuds

//   out << "\n<h1>Nodes Documentation</h1>\n\n";
//   i = UINodeRepository::Begin();
//   while (i != UINodeRepository::End())
//   {
//      node2html(i->first, i->second, out);
//      i++;
//   }
//   out << "\n</body>\n"
//       << "</html>\n";


//}
