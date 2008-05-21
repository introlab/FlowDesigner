#ifndef _GUINOTE_H_
#define _GUINOTE_H_

#include "UINote.h"
#include <string>
#include "UINetwork.h"
#include <gnome.h>

namespace FD {

class GUINote : public UINote {

 private:
  UINetwork *m_network;
  GnomeCanvasGroup *m_group;
  GtkWidget *m_textView;

 public:

  GUINote(const std::string &text, double x, double y, bool visible, UINetwork *net);

  virtual ~GUINote();

  void move(double dx, double dy);

  gint event(GdkEvent *event);

  void update_text();
};

}//namespace FD


#endif
