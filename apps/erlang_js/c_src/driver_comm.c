/*
Copyright (c) 2009 Hypothetical Labs, Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#define COMMAND_SIZE 2

#include <string.h>
#include <strings.h>

#include <erl_driver.h>

#include "driver_comm.h"

inline int read_int32(char **data) {
  char *d = *data;
  int value = ((((int)(((unsigned char*) (d))[0]))  << 24) |
	       (((int)(((unsigned char*) (d))[1]))  << 16) |
	       (((int)(((unsigned char*) (d))[2]))  << 8)  |
	       (((int)(((unsigned char*) (d))[3]))));
  (*data) += 4;
  return value;
}

char *read_command(char **data) {
  char *buf = (char *) driver_alloc(COMMAND_SIZE + 1);
  memset(buf, 0, COMMAND_SIZE + 1);
  memcpy(buf, (const char *) *data, COMMAND_SIZE);
  (*data) += 2;
  return buf;
}

char *read_string(char **data) {
  int length = read_int32(data);
  char *buf = NULL;
  if (length > 0) {
    buf = (char *) driver_alloc(length + 1);
    memset(buf, 0, length + 1);
    memcpy(buf, (const char *) *data, length);
    (*data) += length;
  }
  return buf;
}
