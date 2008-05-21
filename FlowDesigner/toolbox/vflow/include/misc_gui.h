#ifndef MISC_GUI_H
#define MISC_GUI_H

#include <string>
#include <gnome.h>

namespace FD {

std::string ask_string_dialog (const char *question, const char *default_str);

gint close_save_dialog (const char *question);

}//namespace FD

#endif
