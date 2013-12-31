#include <gtk/gtk.h>
#include <stdio.h>

void find_wacom( char* stylus_name, char* eraser_name ) ; 

void initdevice( int* core, int* stylus, int* eraser, 
                 char* corepointername, char* stylusname, char* erasername ); 

void enable_touch( char* touch_name ) ;

void disable_touch( char* touch_name ); 
