// Copyright (C) 1999 Jean-Marc Valin

#include "audioinfo.h"
#include "ObjectParser.h"
#include <string.h>

using namespace std;
using namespace FD;

DECLARE_TYPE(AudioInfo)

void AudioInfo::printOn(ostream &out) const
{
   out << "<AudioInfo " << endl;
   out << "<ortho \"" << ortho << "\" >" << endl;
   if (coarse_endpointed)
   {
      out << "<coarse_start " << coarse_start << ">" << endl;
      out << "<coarse_end " << coarse_end << ">" << endl;
   }
   if (fine_endpointed)
   {
      out << "<fine_start " << fine_start << ">" << endl;
      out << "<fine_start " << fine_start << ">" << endl;
   }
   out << ">\n";
}

void AudioInfo::readFrom (istream &in)
{
   string tag;

   while (1)
   {
      char ch;
      in >> ch;
      if (ch == '>') break;
      else if (ch != '<') 
       throw new ParsingException ("AudioInfo::readFrom : Parse error: '<' expected");
      in >> tag;
      if (tag == "ortho")
      {
         ortho="";
         do {
            in >> ch;
         } while (ch != '"');
         
         in.get(ch);
         while (ch != '"')
         {
            ortho += ch;
            in.get(ch);
         }


      }
      //  in >> ortho;
      else if (tag == "coarse_start")
      {
         in >> coarse_start;
         coarse_endpointed=true;
      } else if (tag == "coarse_end")
      {
         in >> coarse_end;
         coarse_endpointed=true;
      } else if (tag == "fine_start")
      {
         in >> fine_start;
         fine_endpointed=true;
      } else if (tag == "coarse_end")
      {
         in >> fine_end;
         fine_endpointed=true;
      } else
         throw new ParsingException ("AudioInfo::readFrom : unknown argument: " + tag);
      
      if (!in) throw new ParsingException ("AudioInfo::readFrom : Parse error trying to build " + tag);

      in >> tag;
      if (tag != ">") 
         throw new ParsingException ("AudioInfo::readFrom : Parse error: '>' expected ");
   }

}

istream &operator >> (istream &in, AudioInfo &info)
{
   if (!isValidType(in, "AudioInfo")) return in;
   info.readFrom(in);
   return in;
}
