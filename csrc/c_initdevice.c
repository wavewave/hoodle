#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>

void initdevice( int* core, int* stylus, int* eraser, 
                 char* corepointername, char* stylusname, char* erasername
               )
{
  GList* dev_list;
  GdkDevice* device;
  dev_list = gdk_devices_list();
  (*stylus) = 0;
  while (dev_list != NULL) {
    printf ("one device\n"); 
    device = (GdkDevice *)dev_list->data;
    printf(" %d : %s \n", device, device->name );
    if (device != gdk_device_get_core_pointer()) {
      // #ifdef ENABLE_XINPUT_BUGFIX
      gdk_device_set_axis_use(device, 0, GDK_AXIS_IGNORE);
      gdk_device_set_axis_use(device, 1, GDK_AXIS_IGNORE);
      // #endif
      gdk_device_set_mode(device, GDK_MODE_SCREEN);

      printf("This is xinput device %s \n", device -> name);
      if( !strcmp (device->name, stylusname) ) {
        printf("got stylus\n");   
        (*stylus) = (int) device; 
      } 
      if( !strcmp (device->name, erasername) ) { 
        printf("got eraser\n");
        (*eraser) = (int) device;
      } 
    } 
    else { 
      if( !strcmp (device->name, corepointername) ) { 
        printf("got Core Pointer\n"); 
        (*core) = (int) device; 
      } 
    } 
    dev_list = dev_list->next; 
  }

}


// void extEventCanvas( void* canvas ) {
/* Important note: we'd like ONLY the canvas window itself to receive
   XInput events, while its child window in the GDK hierarchy (also
   associated to the canvas widget) receives the core events.
   This way on_canvas_... will get both types of events -- otherwise,
   the proximity detection code in GDK is broken and we'll lose core
   events.
   
   Up to GTK+ 2.10, gtk_widget_set_extension_events() only sets
   extension events for the widget's main window itself; in GTK+ 2.11
   also traverses GDK child windows that belong to the widget
   and sets their extension events too. We want to avoid that.
   So we use gdk_input_set_extension_events() directly on the canvas.
*/
   
/*  // this causes GTK+ 2.11 bugs
  gtk_widget_set_extension_events(GTK_WIDGET(canvas),GDK_EXTENSION_EVENTS_ALL); 
*/

//  gdk_input_set_extension_events(GTK_WIDGET(canvas)->window, GDK_POINTER_MOTION_MASK | GDK_BUTTON_MOTION_MASK | GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK, GDK_EXTENSION_EVENTS_ALL);
//}
