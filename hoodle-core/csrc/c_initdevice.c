#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>

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
  printf("initdevice : corepointername = %s\n", corepointername );
  printf("initdevice : stylusname = %s\n", stylusname );
  printf("initdevice : erasername = %s\n", erasername ); 
  printf("initdevice : touchname = %s\n", touchname );                      

  GList* dev_list;
  GList* dev_list_m;
  GdkDevice* device;

  GdkDisplay* disp = gdk_display_get_default();
  
  GdkDeviceManager *devman = gdk_display_get_device_manager(disp);

  dev_list_m = gdk_device_manager_list_devices(devman, GDK_DEVICE_TYPE_MASTER );
  while( dev_list_m != NULL ) {
    device = (GdkDevice *)dev_list_m->data;
    // printf("device = %d\n", device);
    char* name = gdk_device_get_name(device);
    printf("%s\n", name );
    if( !strcmp (gdk_device_get_name(device), corepointername) ) { 
        (*core) = (int) device; 
    } 
    
    dev_list_m = g_list_next(dev_list_m); 
  }

  //GdkDevice* master = gdk_seat_get_pointer(seat);
  //(*core) = (int)master;
  
    
  /* 
  dev_list = gdk_device_manager_list_devices(devman, GDK_DEVICE_TYPE_SLAVE );
  // #else // GTK3
  // dev_list = gdk_devices_list();
  // #endif // GTK3
  (*stylus) = 0;
  while (dev_list != NULL) {
    device = (GdkDevice *)dev_list->data;
    printf("device = %d\n", device);
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

      // #ifdef GTK3
    dev_list = g_list_next(dev_list); 
    //#else // GTK3
    // dev_list = dev_list->next;
    // #endif // GTK3
    } */


}

// #ifndef GTK3
// void enable_touch( char* touch_name ) 
// {
//   printf("enable touch: %s\n", touch_name ) ; 
// }

// void disable_touch( GdkDrawable *gdkwin, char* touch_name ) 
// {
//   printf("disable touch: %s\n", touch_name ); 
//   
// }
// #endif // GTK3
