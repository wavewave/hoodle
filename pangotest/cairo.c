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

  //pctxt = pango_context_new();
  playout = pango_cairo_create_layout( cr );

  pfd = pango_font_description_new();
  //playout = pango_layout_new( pctxt ) ;

  pango_layout_set_font_description( playout, pfd);
  pango_layout_set_markup( playout, "x<span size=\"0\">a<span letter_spacing=\"20000\">a</span>a</span>y", -1);

  pango_cairo_layout_path( cr, playout );

  pango_cairo_show_layout( cr, playout );

  status = cairo_surface_write_to_png( sfc, "text.png" ) ;
  
  cairo_destroy( cr );
  cairo_surface_destroy( sfc ); 
  
  printf("status = %d\n", status);
}

/*

cxt = Cairo::Context.new(Cairo::ImageSurface.new(100,100))
layout = cxt.create_pango_layout
font = Pango::FontDescription.new('Sans Bold 36')
layout.font_description = font

# This markup gives us space to render an image in the text flow and have it flow like words.
# It is a bit hacky, but I can't figure out how to get ruby+pango+cairo to create arbitrary space
layout.markup = 'x<span size="0">a<span letter_spacing="20000">a</span>a</span>y'

cxt.pango_layout_path(layout) # fails here

cxt.show_pango_layout layout
cxt.target.write_to_png('bug_103.png')
*/
