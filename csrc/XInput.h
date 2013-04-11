//
// WacomWebPlugin - An implementation of the Wacom Web API on Linux.
// Copyright (C) 2013  Zane Ashby <zane.a@demonastery.org>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

//
// XInput Wrapper.
//

#ifndef __XINPUT_WRAPPER_H
#define __XINPUT_WRAPPER_H

// #include "common.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <assert.h>

#include <stdbool.h>

#include <unistd.h>
#include <pthread.h>

#include <X11/Xlib.h>
#include <X11/extensions/XInput.h>
#include <X11/Xutil.h>

#ifdef DEBUG
#define debug printf
#else
#define debug(body, ...)
#endif


// Helper macros, for returning results.
#define XINPUT(name, type) if (IS_IDENTIFIER(""#name)) { type##_TO_NPVARIANT(values->name, *result); return true; }
#define XINPUT_STRING(name) if (IS_IDENTIFIER(""#name)) { STRINGZ_TO_NPVARIANT(strdup(values->name), *result); return true; }

#define VALUATOR_ABSX     0
#define VALUATOR_ABSY     1
#define VALUATOR_PRESSURE 2
#define VALUATOR_TILTX    3
#define VALUATOR_TILTY    4

// extern NPClass BaseClass;

// Just about all of these values should be attainable from XInput.
typedef struct xinput_values_t {
  bool isWacom;
  bool isEraser;
  float pressure;
  long posX;
  long posY;
  float sysX;
  float sysY;
  long tabX;
  long tabY;
  float rotationDeg;
  float rotationRad;
  float tiltX;
  float tiltY;
  float tangentialPressure;
  long pointerType;
  char tabletModel[256];
  char tabletModelID[256];
} xinput_values_t;

// Any other global XInput wrapper state.
typedef struct xinput_state_t {
  int refcount;
  pthread_t thread;

  int screen_width, screen_height;

  // Store range of values for each valuator.
  float absx_min, absx_max;
  float absy_min, absy_max;
  float pressure_min, pressure_max;
  float tilt_min, tilt_max;

  int motionType;
  int buttonPressType;
  int buttonReleaseType;
  int proximityInType;
  int proximityOutType;
} xinput_state_t;

//
// Functions
//

void xinput_start();
void xinput_stop();
xinput_values_t *xinput_getValues();

#endif
