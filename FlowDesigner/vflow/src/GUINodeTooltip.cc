// Copyright (C) 2001 Jean-Marc Valin

#include "GUINodeTooltip.h"
#include "misc.h"

GUINodeTooltip::GUINodeTooltip(GUINode *_node)
   : node(_node)
{
   GnomeCanvasGroup* nodeGroup = node->getGroup();
   GnomeCanvasItem *item;
   double x1,x2,y1,y2;
   double xx1,xx2,yy1,yy2;
   group = GNOME_CANVAS_GROUP (gnome_canvas_item_new (nodeGroup,
                                                      gnome_canvas_group_get_type(),
                                                      "x", 10.0,
                                                      "y", -40.0,
                                                      NULL));
   item = gnome_canvas_item_new(group,
				gnome_canvas_text_get_type(),
				"x", 0.0,
				"y", -32.0,
				"text", node->getName().c_str(),
				"anchor", GTK_ANCHOR_CENTER,
				"fill_color", "black",
				"font", "fixed",
				NULL);
   
   gnome_canvas_item_get_bounds(item, &x1,&y1, &x2, &y2);
   
   
   item = gnome_canvas_item_new(group,
				gnome_canvas_text_get_type(),
				"x", 0.0,
				"y", -20.0,
				"text", node->getType().c_str(),
				"anchor", GTK_ANCHOR_CENTER,
				"fill_color", "black",
				"font", "fixed",
				NULL);
   gnome_canvas_item_get_bounds(item, &xx1,&yy1, &xx2, &yy2);
   x1=min(x1,xx1);
   x2=max(x2,xx2);
   y1=min(y1,yy1);
   y2=max(y2,yy2);


   item = gnome_canvas_item_new(group,
				gnome_canvas_text_get_type(),
				"x", 0.0,
				"y", 0.0,
				"text", node->getComments().c_str(),
				"anchor", GTK_ANCHOR_CENTER,
				"fill_color", "black",
				"font", "fixed",
				NULL);
   gnome_canvas_item_get_bounds(item, &xx1,&yy1, &xx2, &yy2);
   x1=min(x1,xx1);
   x2=max(x2,xx2);
   y1=min(y1,yy1);
   y2=max(y2,yy2);


   item = gnome_canvas_item_new(group,
				gnome_canvas_rect_get_type(),
				"x1", x1-5,
				"y1", y1-5,
				"x2", x2+5,
				"y2", y2+5,
				"fill_color_rgba", 0xe0e03020,
				"outline_color", "black",
				"width_units", 1.0,
				NULL);
   gnome_canvas_item_lower_to_bottom(item);

}

GUINodeTooltip::~GUINodeTooltip()
{
   gtk_object_destroy(GTK_OBJECT(group));
}
