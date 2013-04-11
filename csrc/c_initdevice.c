#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>

#include "XInput.h"

//
// Globals.
//

// Holds the values for the Wacom API.
xinput_values_t g_xinput_values = {
  .isWacom = true,
};

// Holds XInput wrapper state.
xinput_state_t g_xinput_state = {
  .refcount = 0,

  .thread = 0,

  .screen_width = 0,
  .screen_height = 0,

  // Valuator ranges.
  .absx_min = 0,
  .absx_min = 0,
  .absy_max = 1,
  .absy_max = 1,
  .pressure_min = 0,
  .pressure_max = 1,
  .tilt_min = 0,
  .tilt_max = 1,

  // XInput event types.
  .motionType = -1,
  .buttonPressType = -1,
  .buttonReleaseType = -1,
  .proximityInType = -1,
  .proximityOutType = -1,
};

//
// Functions.
//

static bool xinput_findDevices(Display *display, XDeviceInfo *stylus_info, XDeviceInfo *eraser_info)
{
  int found = 0;

  int num;
  XDeviceInfo *devices = XListInputDevices(display, &num);

  Atom stylus = XInternAtom(display, "STYLUS", false);
  Atom eraser = XInternAtom(display, "ERASER", false);

  for (int i = 0; i < num; i++) {
    debug("Got device \"%s\"\n", devices[i].name);

    if (devices[i].type == stylus) {
      found++;

      // Fill tablet model here.
      snprintf(g_xinput_values.tabletModel, sizeof(g_xinput_values.tabletModel), "%s", devices[i].name);
      snprintf(g_xinput_values.tabletModelID, sizeof(g_xinput_values.tabletModelID), "%s", devices[i].name);

      debug("Found stylus.\n");

      memcpy(stylus_info, &devices[i], sizeof(XDeviceInfo));

      XAnyClassPtr any = (XAnyClassPtr)devices[i].inputclassinfo;

      for (int j = 0; j < devices[i].num_classes; j++) {
        if (any->class == ValuatorClass) {
          XValuatorInfoPtr v = (XValuatorInfoPtr)any;

          // X.
          g_xinput_state.absx_min = v->axes[VALUATOR_ABSX].min_value;
          g_xinput_state.absx_min = v->axes[VALUATOR_ABSX].max_value;
          debug("X valuator range: min=%d, max=%d\n", v->axes[VALUATOR_ABSX].min_value, v->axes[VALUATOR_ABSX].max_value);

          // Y.
          g_xinput_state.absy_min = v->axes[VALUATOR_ABSY].min_value;
          g_xinput_state.absy_min = v->axes[VALUATOR_ABSY].max_value;
          debug("Y valuator range: min=%d, max=%d\n", v->axes[VALUATOR_ABSY].min_value, v->axes[VALUATOR_ABSY].max_value);

          // Pressure.
          g_xinput_state.pressure_min = v->axes[VALUATOR_PRESSURE].min_value;
          g_xinput_state.pressure_max = v->axes[VALUATOR_PRESSURE].max_value;
          debug("Pressure valuator range: min=%d, max=%d\n", v->axes[VALUATOR_PRESSURE].min_value, v->axes[VALUATOR_PRESSURE].max_value);

          // Tilt. Assuming that the range is the same for X and Y...
          g_xinput_state.tilt_min = v->axes[VALUATOR_TILTX].min_value;
          g_xinput_state.tilt_max = v->axes[VALUATOR_TILTX].max_value;
          debug("Tilt valuator range: min=%d, max=%d\n", v->axes[VALUATOR_TILTX].min_value, v->axes[VALUATOR_TILTX].max_value);

          break;
        }

        any = (XAnyClassPtr)((char *)any + any->length);
      }
    } else if (devices[i].type == eraser) {
      found++;

      debug("Found eraser.\n");

      memcpy(eraser_info, &devices[i], sizeof(XDeviceInfo));
    }
  }

  XFreeDeviceList(devices);

  return (found == 2);
}


void find_wacom( char* stylus_name, char* eraser_name) 
{
  Display *display = XOpenDisplay(NULL); 
  assert(display != NULL);

  g_xinput_state.screen_width = XWidthOfScreen(DefaultScreenOfDisplay(display));
  g_xinput_state.screen_height = XHeightOfScreen(DefaultScreenOfDisplay(display));

  XDeviceInfo stylus_info, eraser_info;

  if (!xinput_findDevices(display, &stylus_info, &eraser_info)) {
    fprintf(stderr, "Couldn't find device.\n");
  }

  printf("found stylus=%s \n", stylus_info.name ); 
  printf("found eraser=%s \n", eraser_info.name ); 
  strcpy(stylus_name, stylus_info.name);
  strcpy(eraser_name, eraser_info.name); 

  return ; 
}

void initdevice( int* core, int* stylus, int* eraser, 
                 char* corepointername, char* stylusname, char* erasername
               )
{
  // xinput_start(); 

  printf("initdevice : stylusname = %s\n", stylusname );
  printf("initdevice : erasername = %s\n", erasername ); 

  GList* dev_list;
  GdkDevice* device;
  dev_list = gdk_devices_list();
  (*stylus) = 0;
  while (dev_list != NULL) {
    // printf ("one device\n"); 
    device = (GdkDevice *)dev_list->data;
    // printf(" %d : %s \n", device, device->name );
    if (device != gdk_device_get_core_pointer()) {
      // #ifdef ENABLE_XINPUT_BUGFIX
      gdk_device_set_axis_use(device, 0, GDK_AXIS_IGNORE);
      gdk_device_set_axis_use(device, 1, GDK_AXIS_IGNORE);
      // #endif
      gdk_device_set_mode(device, GDK_MODE_SCREEN);

      // printf("This is xinput device %s \n", device -> name);
      if( !strcmp (device->name, stylusname) ) {
        // printf("got stylus\n");   
        (*stylus) = (int) device; 
      } 
      if( !strcmp (device->name, erasername) ) { 
        // printf("got eraser\n");
        (*eraser) = (int) device;
      } 
    } 
    else { 
      if( !strcmp (device->name, corepointername) ) { 
        // printf("got Core Pointer\n"); 
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
