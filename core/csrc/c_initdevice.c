#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>

// NOTE: This application only works with gtk+-3.0.

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
    printf("device = %d\n", device);
    char* name = gdk_device_get_name(device);
    printf("%s\n", name );
    if( !strcmp (gdk_device_get_name(device), corepointername) ) {
        (*core) = (int) device;
    }

    dev_list_m = g_list_next(dev_list_m);
  }


}
