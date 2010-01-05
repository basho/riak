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
