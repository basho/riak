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

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <erl_driver.h>

#include "spidermonkey.h"

/* The class of the global object. */
static JSClass global_class = {
    "global", JSCLASS_GLOBAL_FLAGS,
    JS_PropertyStub, JS_PropertyStub, JS_PropertyStub, JS_PropertyStub,
    JS_EnumerateStub, JS_ResolveStub, JS_ConvertStub, JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

char *copy_string(const char *source) {
  int size = strlen(source) + 1;
  char *retval = driver_alloc(size);
  memset(retval, 0, size);
  strncpy(retval, source, size - 1);
  return retval;
}

char *copy_jsstring(JSString *source) {
  char *buf = JS_GetStringBytes(source);
  return copy_string(buf);
}

inline void begin_request(spidermonkey_vm *vm) {
  JS_SetContextThread(vm->context);
  JS_BeginRequest(vm->context);
}

inline void end_request(spidermonkey_vm *vm) {
  JS_EndRequest(vm->context);
  JS_ClearContextThread(vm->context);
}

void on_error(JSContext *context, const char *message, JSErrorReport *report) {
  if (report->flags & JSREPORT_EXCEPTION) {
    spidermonkey_error *sm_error = (spidermonkey_error *) driver_alloc(sizeof(spidermonkey_error));
    sm_error->msg = copy_string(message);
    sm_error->lineno = report->lineno;
    sm_error->offending_source = copy_string(report->linebuf);
    JS_SetContextPrivate(context, sm_error);
  }
}

void write_timestamp(FILE *fd) {
  struct tm *tmp;
  time_t t;

  t = time(NULL);
  tmp = localtime(&t); /* or gmtime, if you want GMT^H^H^HUTC */
  fprintf(fd, "%02d/%02d/%04d (%02d:%02d:%02d): ",
	  tmp->tm_mon+1, tmp->tm_mday, tmp->tm_year+1900,
	  tmp->tm_hour, tmp->tm_min, tmp->tm_sec);
}

JSBool js_log(JSContext *cx, uintN argc, jsval *vp) {
  if (argc != 2) {
    JS_SET_RVAL(cx, vp, JSVAL_FALSE);
  }
  else {
    jsval *argv = JS_ARGV(cx, vp);
    jsval jsfilename = argv[0];
    jsval jsoutput = argv[1];
    char *filename = JS_GetStringBytes(JS_ValueToString(cx, jsfilename));
    char *output = JS_GetStringBytes(JS_ValueToString(cx, jsoutput));
    FILE *fd = fopen(filename, "a+");
    if (fd != NULL) {
      write_timestamp(fd);
      fwrite(output, 1, strlen(output), fd);
      fwrite("\n", 1, strlen("\n"), fd);
      fclose(fd);
      JS_SET_RVAL(cx, vp, JSVAL_TRUE);
    }
    else {
      JS_SET_RVAL(cx, vp, JSVAL_FALSE);
    }
  }
  return JSVAL_TRUE;
}

spidermonkey_vm *sm_initialize() {
  spidermonkey_vm *vm = (spidermonkey_vm*) driver_alloc(sizeof(spidermonkey_vm));
  vm->runtime = JS_NewRuntime(MAX_GC_SIZE);
  JS_SetGCParameter(vm->runtime, JSGC_MAX_BYTES, CONTEXT_HEAP_SIZE);
  JS_SetGCParameter(vm->runtime, JSGC_MAX_MALLOC_BYTES, LAST_DITCH_GC_THRESHOLD);
  vm->context = JS_NewContext(vm->runtime, CONTEXT_THREAD_STACK_SIZE);
  begin_request(vm);
  JS_SetOptions(vm->context, JSOPTION_VAROBJFIX);
  JS_SetOptions(vm->context, JSOPTION_STRICT);
  JS_SetOptions(vm->context, JSOPTION_COMPILE_N_GO);
  JS_SetOptions(vm->context, JSVERSION_LATEST);
  vm->global = JS_NewObject(vm->context, &global_class, NULL, NULL);
  JS_InitStandardClasses(vm->context, vm->global);
  JS_SetErrorReporter(vm->context, on_error);
  JSNative *funptr = (JSNative *) *js_log;
  JS_DefineFunction(vm->context, JS_GetGlobalObject(vm->context), "ejsLog", (JSNative *) funptr,
		    0, JSFUN_FAST_NATIVE);
  end_request(vm);
  return vm;
}

void sm_stop(spidermonkey_vm *vm) {
  JS_SetContextThread(vm->context);
  JS_DestroyContext(vm->context);
  JS_DestroyRuntime(vm->runtime);
  driver_free(vm);
}

void sm_shutdown() {
  JS_ShutDown();
}

char *error_to_json(const spidermonkey_error *error) {
  /* Allocate 1K to build error (hopefully that's enough!) */
  int size = sizeof(char) * 1024;
  char *retval = (char *) driver_alloc(size);
  snprintf(retval, size, "{\"error\": {\"lineno\": %d, \"message\": \"%s\", \"source\": \"%s\"}}", error->lineno,
	   error->msg, error->offending_source);
  return retval;
}

void free_error(spidermonkey_error *error) {
  driver_free(error->offending_source);
  driver_free(error->msg);
  driver_free(error);
}

char *sm_eval(spidermonkey_vm *vm, const char *filename, const char *code, int handle_retval) {
  char *retval = NULL;
  JSScript *script;
  jsval result;

  begin_request(vm);
  script = JS_CompileScript(vm->context,
			    vm->global,
			    code, strlen(code),
			    filename, 1);
  spidermonkey_error *error = (spidermonkey_error *) JS_GetContextPrivate(vm->context);
  if (error == NULL) {
    JS_ClearPendingException(vm->context);
    JS_ExecuteScript(vm->context, vm->global, script, &result);
    if (handle_retval) {
      if (JSVAL_IS_STRING(result)) {
	JSString *str = JS_ValueToString(vm->context, result);
	retval = copy_jsstring(str);
      }
      else {
	retval = copy_string("{\"error\": \"non-JSON return value\"}");
      }
    }
    JS_DestroyScript(vm->context, script);
  }
  else {
    retval = error_to_json(error);
    free_error(error);
    JS_SetContextPrivate(vm->context, NULL);
  }
  JS_MaybeGC(vm->context);
  end_request(vm);
  return retval;
}
