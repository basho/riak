/* author Kevin Smith <ksmith@basho.com>
   copyright 2009-2010 Basho Technologies

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

#ifndef __SPIDERMONKEY_INTERFACE_
#define __SPIDERMONKEY_INTERFACE_

#include "jsapi.h"

typedef struct _spidermonkey_error_t {
  unsigned int lineno;
  char *msg;
  char *offending_source;
} spidermonkey_error;

typedef struct _spidermonkey_vm_t {
  JSRuntime* runtime;
  JSContext* context;
  JSObject* global;
} spidermonkey_vm;

/* Bytes to allocate before GC */
#define MAX_GC_SIZE 1024 * 1024

/* 8K stack size for each context */
#define CONTEXT_THREAD_STACK_SIZE 8192

/* 8MB heap for each context */
#define CONTEXT_HEAP_SIZE 8 * 1024 * 1024

/* 8MB last ditch GC threshold */
#define LAST_DITCH_GC_THRESHOLD CONTEXT_HEAP_SIZE

spidermonkey_vm *sm_initialize();

void sm_stop(spidermonkey_vm *vm);

void sm_shutdown();

char *sm_eval(spidermonkey_vm *vm, const char *filename, const char *code, int handle_retval);

#endif
