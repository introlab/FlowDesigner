/* Image item type for GnomeCanvas widget
 *
 * GnomeCanvas is basically a port of the Tk toolkit's most excellent canvas widget.  Tk is
 * copyrighted by the Regents of the University of California, Sun Microsystems, and other parties.
 *
 * Copyright (C) 1998 The Free Software Foundation
 *
 * Author: Federico Mena <federico@nuclecu.unam.mx>
 */

#include "canvas-background.h"




static void gnome_canvas_background_class_init (GnomeCanvasBackgroundClass *class);
static void gnome_canvas_background_init       (GnomeCanvasBackground      *background);
static void gnome_canvas_background_destroy    (GtkObject             *object);
static void gnome_canvas_background_set_arg    (GtkObject             *object,
					   GtkArg                *arg,
					   guint                  arg_id);
static void gnome_canvas_background_get_arg    (GtkObject             *object,
					   GtkArg                *arg,
					   guint                  arg_id);

static void   gnome_canvas_background_update      (GnomeCanvasItem *item, double *affine, ArtSVP *clip_path, int flags);
static void   gnome_canvas_background_realize     (GnomeCanvasItem *item);
static void   gnome_canvas_background_unrealize   (GnomeCanvasItem *item);
static void   gnome_canvas_background_draw        (GnomeCanvasItem *item, GdkDrawable *drawable,
					      int x, int y, int width, int height);
static double gnome_canvas_background_point       (GnomeCanvasItem *item, double x, double y,
					      int cx, int cy, GnomeCanvasItem **actual_item);
static void   gnome_canvas_background_translate   (GnomeCanvasItem *item, double dx, double dy);
static void   gnome_canvas_background_bounds      (GnomeCanvasItem *item, double *x1, double *y1, double *x2, double *y2);
static void   gnome_canvas_background_render      (GnomeCanvasItem *item, GnomeCanvasBuf *buf);

//static ArtPixBuf * pixbuf_from_imlib_background (GdkImlibBackground *im);

static GnomeCanvasItemClass *parent_class;


GtkType
gnome_canvas_background_get_type (void)
{
	static GtkType background_type = 0;

	if (!background_type) {
		GtkTypeInfo background_info = {
			"GnomeCanvasBackground",
			sizeof (GnomeCanvasBackground),
			sizeof (GnomeCanvasBackgroundClass),
			(GtkClassInitFunc) gnome_canvas_background_class_init,
			(GtkObjectInitFunc) gnome_canvas_background_init,
			NULL, /* reserved_1 */
			NULL, /* reserved_2 */
			(GtkClassInitFunc) NULL
		};

		background_type = gtk_type_unique (gnome_canvas_item_get_type (), &background_info);
	}

	return background_type;
}

static void
gnome_canvas_background_class_init (GnomeCanvasBackgroundClass *class)
{
	GtkObjectClass *object_class;
	GnomeCanvasItemClass *item_class;

	object_class = (GtkObjectClass *) class;
	item_class = (GnomeCanvasItemClass *) class;

	parent_class = gtk_type_class (gnome_canvas_item_get_type ());


	object_class->destroy = gnome_canvas_background_destroy;
	object_class->set_arg = gnome_canvas_background_set_arg;
	object_class->get_arg = gnome_canvas_background_get_arg;

	item_class->update = gnome_canvas_background_update;
	item_class->realize = gnome_canvas_background_realize;
	item_class->unrealize = gnome_canvas_background_unrealize;
	item_class->draw = gnome_canvas_background_draw;
	item_class->point = gnome_canvas_background_point;
	item_class->translate = gnome_canvas_background_translate;
	item_class->bounds = gnome_canvas_background_bounds;
	item_class->render = gnome_canvas_background_render;


}

static void
gnome_canvas_background_init (GnomeCanvasBackground *background)
{
   GnomeCanvasItem *item;

   item = GNOME_CANVAS_ITEM(background);
   item->x1 = -10000;
   item->y1 = -10000;
   item->x2 =  10000;
   item->y2 =  10000;
}

static void gnome_canvas_background_destroy (GtkObject *object)
{
   //GnomeCanvasBackground *background;

        g_return_if_fail (object != NULL);
        g_return_if_fail (GNOME_IS_CANVAS_BACKGROUND (object));

        //background = GNOME_CANVAS_IMAGE (object);


        if (GTK_OBJECT_CLASS (parent_class)->destroy)
                (* GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}


static void
gnome_canvas_background_set_arg (GtkObject *object, GtkArg *arg, guint arg_id)
{

}

static void
gnome_canvas_background_get_arg (GtkObject *object, GtkArg *arg, guint arg_id)
{

}

static void
gnome_canvas_background_update (GnomeCanvasItem *item, double *affine, ArtSVP *clip_path, int flags)
{
if (parent_class->update)
                (* parent_class->update) (item, affine, clip_path, flags);

}

static void
gnome_canvas_background_realize (GnomeCanvasItem *item)
{
	GnomeCanvasBackground *background;

	background = GNOME_CANVAS_BACKGROUND (item);

	if (parent_class->realize)
		(* parent_class->realize) (item);

//	if (!item->canvas->aa)
//		background->gc = gdk_gc_new (item->canvas->layout.bin_window);
}

static void
gnome_canvas_background_unrealize (GnomeCanvasItem *item)
{
	GnomeCanvasBackground *background;

	background = GNOME_CANVAS_BACKGROUND (item);

	if (parent_class->unrealize)
		(* parent_class->unrealize) (item);
}



static void
gnome_canvas_background_draw (GnomeCanvasItem *item, GdkDrawable *drawable,
			 int x, int y, int width, int height)
{

}


static void
gnome_canvas_background_translate (GnomeCanvasItem *item, double dx, double dy)
{
}

static void
gnome_canvas_background_bounds (GnomeCanvasItem *item, double *x1, double *y1, double *x2, double *y2)
{
   *x1 = -10000000;
   *y1 = -10000000;
   *x2 =  10000000;
   *y2 =  10000000;
}

static double
gnome_canvas_background_point (GnomeCanvasItem *item, double x, double y,
                          int cx, int cy, GnomeCanvasItem **actual_item)
{
   return 0.0;
}


static void   gnome_canvas_background_render      (GnomeCanvasItem *item, GnomeCanvasBuf *buf)
{
}
