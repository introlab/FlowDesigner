// Copyright (C) 2001 Jean-Marc Valin

#ifndef GUINODE_TOOLTIP_H
#define GUINODE_TOOLTIP_H

#include "GUINode.h"
#include <gnome.h>

class GUINodeTooltip {
  protected:
   GUINode *node;

   GnomeCanvasGroup *group;
  
   GnomeCanvasItem *tooltipRect;
   GnomeCanvasItem *tooltipText;

  public:
   GUINodeTooltip(GUINode *_node);
};


#endif
