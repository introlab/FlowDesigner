// Copyright (C) 2001 Jean-Marc Valin

#include "GUINodeTooltip.h"

GUINodeTooltip::GUINodeTooltip(GUINode *_node)
   : node(_node)
{
   GnomeCanvasGroup* nodeGroup = node->getGroup();

   group = GNOME_CANVAS_GROUP (gnome_canvas_item_new (nodeGroup,
                                                      gnome_canvas_group_get_type(),
                                                      "x", 0.0,
                                                      "y", -30.0,
                                                      NULL));
   tooltipText = gnome_canvas_item_new(group,
				 gnome_canvas_text_get_type(),
                                 "x", 0.0,
                                 "y", 0.0,
                                 "text", "node comments",
                                 "anchor", GTK_ANCHOR_CENTER,
                                 "fill_color", "blue",
                                 "font", "fixed",
                                 NULL);

}
