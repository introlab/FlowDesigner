// Copyright (C) 2001 Jean-Marc Valin

#include "GUINodeTooltip.h"
#include "misc.h"
#include <sstream>
#include "UINodeParameters.h"
#include "GUINetwork.h"
#include <map>
#include "UIDocument.h"

GUINodeTooltip::GUINodeTooltip(GUINode *_node)
   : node(_node)
{
   GnomeCanvasGroup* nodeGroup = dynamic_cast<GUINetwork*>(node->getNetwork())->getGroup();
   GnomeCanvasItem *item;
   double x1,x2,y1,y2;
   double xx1,xx2,yy1,yy2;
   double xpos, ypos;
   node->getPos(xpos,ypos);
   group = GNOME_CANVAS_GROUP (gnome_canvas_item_new (nodeGroup,
                                                      gnome_canvas_group_get_type(),
                                                      "x", xpos+10.0,
                                                      "y", ypos-15.0,
                                                      NULL));

   ostringstream tooltext;
   vector<ParameterText *> &params = node->getParameters()->get_textParams();
   tooltext << "Name: " << node->getName() << endl;
   tooltext << "Type: " << node->getType();
   //tooltext << "Parameters:";
   for (int i=0;i<params.size();i++)
   {
      if (params[i]->value != "")
	 tooltext << endl << params[i]->name << ": " << params[i]->value << " (" << params[i]->type << ")";
      else
	 tooltext << endl << params[i]->name << ":"; 
   }
   if (node->getComments() != "")
   {
      tooltext << endl;
      tooltext << node->getComments();
   } else {
      map<string, SubnetInfo *>::iterator iter;
      iter = UIDocument::externalDocInfo.find(node->getType());
      if (iter != UIDocument::externalDocInfo.end())
      {
	 string &desc = iter->second->description;
	 DYN_VEC(char, desc.size()+1, sdesc);
	 strcpy(sdesc, desc.c_str());
	 char *ptr=sdesc;
	 char *end=sdesc;
	 bool st=false;
	 for (int i=0;i<desc.size();i++)
	 {
	    if (isgraph(sdesc[i]))
	    {
	       if (!st)
	       {
		  ptr=sdesc+i;
		  end=sdesc+i+1;
		  st=true;
	       } else {
		  end=sdesc+i+1;
	       }
	    }
	 }
	 end[0]=0;
	 tooltext << endl << ptr;
	 //tooltext << endl << "\"" << iter->second->description << "\"";
      }
   }
   tooltipText = gnome_canvas_item_new(group,
				gnome_canvas_text_get_type(),
				"x", 0.0,
				"y", -32.0,
				"text", tooltext.str().c_str(),
				"anchor", GTK_ANCHOR_SOUTH,
				"fill_color", "black",
				"font", "fixed",
				NULL);
   
   gnome_canvas_item_get_bounds(tooltipText, &x1,&y1, &x2, &y2);



   tooltipRect = gnome_canvas_item_new(group,
				gnome_canvas_rect_get_type(),
				"x1", x1-5,
				"y1", y1-5,
				"x2", x2+5,
				"y2", y2+5,
				       //"fill_color_rgba", 0xe0e03020,
				       //"fill_color_rgba", 0xe0e06020,
				"fill_color_rgba", 0xf0f05520,
				"outline_color", "black",
				"width_units", 1.0,
				NULL);
   gnome_canvas_item_raise_to_top(tooltipText);

}

GUINodeTooltip::~GUINodeTooltip()
{
   gtk_object_destroy(GTK_OBJECT(group));
}
