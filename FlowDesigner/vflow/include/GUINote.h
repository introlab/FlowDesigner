#ifndef _GUINOTE_H_
#define _GUINOTE_H_

#include "UINote.h"
#include <string>
#include "UINetwork.h"
#include <gnome.h>

class GUINote : public UINote {

 private:
  UINetwork *m_network;
  GnomeCanvasGroup *m_group;
  bool m_dragging;
   
 public:
  
  GUINote(const std::string &text, double x, double y, bool visible, UINetwork *net);

  void move(double dx, double dy);

  gint event(GdkEvent *event);
};


#endif
