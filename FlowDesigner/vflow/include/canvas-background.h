/* Copyright (C) 2001 Jean-Marc Valin */

#ifndef GNOME_CANVAS_BACKGROUND_H
#define GNOME_CANVAS_BACKGROUND_H

#include <gnome.h>


BEGIN_GNOME_DECLS




#define GNOME_TYPE_CANVAS_BACKGROUND            (gnome_canvas_background_get_type ())
#define GNOME_CANVAS_BACKGROUND(obj)            (GTK_CHECK_CAST ((obj), GNOME_TYPE_CANVAS_BACKGROUND, GnomeCanvasBackground))
#define GNOME_CANVAS_BACKGROUND_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GNOME_TYPE_CANVAS_BACKGROUND, GnomeCanvasBackgroundClass))
#define GNOME_IS_CANVAS_BACKGROUND(obj)         (GTK_CHECK_TYPE ((obj), GNOME_TYPE_CANVAS_BACKGROUND))
#define GNOME_IS_CANVAS_BACKGROUND_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GNOME_TYPE_CANVAS_BACKGROUND))


typedef struct _GnomeCanvasBackground GnomeCanvasBackground;
typedef struct _GnomeCanvasBackgroundClass GnomeCanvasBackgroundClass;

struct _GnomeCanvasBackground {
	GnomeCanvasItem item;
};

struct _GnomeCanvasBackgroundClass {
	GnomeCanvasItemClass parent_class;
};


/* Standard Gtk function */
GtkType gnome_canvas_background_get_type (void);


END_GNOME_DECLS

#endif
