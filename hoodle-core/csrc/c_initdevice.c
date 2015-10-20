#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>

#ifndef GTK3
#include "XInput.h"
#endif // GTK3

#ifndef GTK3
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
  else { 
    printf("found stylus=%s \n", stylus_info.name ); 
    printf("found eraser=%s \n", eraser_info.name ); 
    strcpy(stylus_name, stylus_info.name);
    strcpy(eraser_name, eraser_info.name); 
  }
  return ; 
}
#endif // not GTK3

void initdevice ( int* core
                , int* stylus
                , int* eraser
		, int* touch 
                , char* corepointername
                , char* stylusname
                , char* erasername
		, char* touchname
                )
{
  printf("initdevice : stylusname = %s\n", stylusname );
  printf("initdevice : erasername = %s\n", erasername ); 
  printf("initdevice : touchname = %s\n", touchname );                      

  GList* dev_list;
  GdkDevice* device;
#ifdef GTK3
  GdkDisplay* disp = gdk_display_get_default();
  GdkDeviceManager *devman = gdk_display_get_device_manager(disp);

  dev_list = gdk_device_manager_list_devices(devman, GDK_DEVICE_TYPE_SLAVE );
#else // GTK3
  dev_list = gdk_devices_list();
#endif // GTK3
  (*stylus) = 0;
  while (dev_list != NULL) {
    device = (GdkDevice *)dev_list->data;
    //if ( gdk_device_get_source(device) != GDK_SOURCE_MOUSE ) {
      gdk_device_set_axis_use(device, 0, GDK_AXIS_IGNORE);
      gdk_device_set_axis_use(device, 1, GDK_AXIS_IGNORE);
      gdk_device_set_mode(device, GDK_MODE_SCREEN);

      if( !strcmp (gdk_device_get_name(device), stylusname) ) {
        (*stylus) = (int) device; 
      } 
      if( !strcmp (gdk_device_get_name(device), erasername) ) { 
        (*eraser) = (int) device;
      } 
      if( !strcmp (gdk_device_get_name(device), touchname) ) {
        (*touch) = (int) device;
      } 
      //} 
      // else { 
      if( !strcmp (gdk_device_get_name(device), corepointername) ) { 
        (*core) = (int) device; 
      } 
      //} 

#ifdef GTK3
    dev_list = g_list_next(dev_list); 
#else // GTK3
    dev_list = dev_list->next;
#endif // GTK3
  }


}

#ifndef GTK3
void enable_touch( char* touch_name ) 
{
  printf("enable touch: %s\n", touch_name ) ; 
}

void disable_touch( GdkDrawable *gdkwin, char* touch_name ) 
{
  printf("disable touch: %s\n", touch_name ); 
  
}
#endif // GTK3
