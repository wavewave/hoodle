#include <cairo.h>
#include <pango/pango.h>
#include <pango/pangocairo.h>
#include <stdio.h>


int main( int argc, char** argv )
{
  cairo_t* cr;
  cairo_surface_t* sfc;
  PangoContext* pctxt;
  PangoLayout* playout;
  PangoFontDescription* pfd;
  int status;
  
  sfc = cairo_image_surface_create( CAIRO_FORMAT_ARGB32, 100, 100 ) ;
  cr = cairo_create( sfc );
  playout = pango_cairo_create_layout( cr );
  pfd = pango_font_description_new();
  pango_layout_set_font_description( playout, pfd);
  pango_layout_set_markup( playout, "x<span size=\"0\">a<span letter_spacing=\"20000\">a</span>a</span>y", -1);
  pango_cairo_layout_path( cr, playout );
  pango_cairo_show_layout( cr, playout );
  status = cairo_surface_write_to_png( sfc, "text.png" ) ;
  cairo_destroy( cr );
  cairo_surface_destroy( sfc ); 
  
  printf("status = %d\n", status);
}
