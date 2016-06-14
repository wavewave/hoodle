#include <cairo.h>
#include <pango/pango.h>
#include <pango/pangocairo.h>
#include <stdio.h>
#include <gtk/gtk.h>


void draw_rectangle( cairo_t* cr )
{
  cairo_set_source_rgb( cr, 0.6, 0.6, 0.6 ) ;
  cairo_rectangle(cr , 20, 20, 80, 86) ;
  cairo_fill (cr );
  
}


void draw_text( cairo_t* cr )
{

  PangoContext* pctxt;
  PangoLayout* playout;
  PangoFontDescription* pfd;
  int status;

  playout = pango_cairo_create_layout( cr );
  pfd = pango_font_description_new();
  pango_layout_set_font_description( playout, pfd);
  pango_layout_set_markup( playout, "x<span size=\"0\">a<span letter_spacing=\"20000\">a</span>a</span>y", -1);
  pango_cairo_layout_path( cr, playout );
  pango_cairo_show_layout( cr, playout );
  status = cairo_status( cr );
  printf("status = %d\n", status);
}


static gboolean
on_draw(GtkWidget *widget, cairo_t *cr, gpointer data)
{
  draw_rectangle( cr );
  draw_text( cr );

  return FALSE;
} 

static void activate( GtkApplication *app, gpointer user_data )
{
  GtkWidget *window;
  GtkWidget *frame;
  GtkWidget *drawing_area;

  window = gtk_application_window_new ( app );
  
  g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL );

  gtk_container_set_border_width( GTK_CONTAINER(window), 8);
  frame = gtk_frame_new(NULL);
  gtk_frame_set_shadow_type( GTK_FRAME(frame), GTK_SHADOW_IN );
  gtk_container_add (GTK_CONTAINER(window), frame );

  drawing_area = gtk_drawing_area_new ();
  gtk_widget_set_size_request (drawing_area, 100,100 );
  gtk_container_add (GTK_CONTAINER(frame), drawing_area) ;

  g_signal_connect(drawing_area, "draw", G_CALLBACK(on_draw), NULL );
  
  gtk_widget_show_all(window);
  
}

int main( int argc, char* argv [] )
{
  GtkApplication* app;
  int status;
  app = gtk_application_new ("org.gtk.example", G_APPLICATION_FLAGS_NONE );
  g_signal_connect (app, "activate", G_CALLBACK(activate), NULL );
  status = g_application_run(G_APPLICATION(app), argc, argv );
  g_object_unref(app);
  return status;
  
}

