#ifndef MISC_GUI_H
#define MISC_GUI_H

#include <string>
#include <gnome.h>


std::string ask_string_dialog (const char *question, const char *default_str);

gint close_save_dialog (const char *question);

#endif
