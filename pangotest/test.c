#include <cairo.h>
#include <pango/pango.h>
#include <stdio.h>


int main( int argc, char** argv )
{
  cairo_t* cr;
  cairo_surface_t* sfc;

  sfc = cairo_image_surface_create( CAIRO_FORMAT_ARGB32, 100, 100 ) ;
  cr = cairo_create( sfc );
  cairo_destroy( cr );
  cairo_surface_destroy( sfc );
  
  printf("hello \n");
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
