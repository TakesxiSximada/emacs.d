#include <stdio.h>
#include <stdlib.h>

#include "scheme.h"
#include "scheme-private.h"

#define car(p) ((p)->_object._cons._car)

static pointer sch_system(scheme *sc, pointer args) {
  pointer cmd = car(args);
  if (!is_string(cmd)) {
    return mk_symbol(sc, "#f");
  }

  const char *cmd_str = string_value(cmd);
  int ret = system(cmd_str);
  return mk_integer(sc, ret);
}

int init_libprocess (scheme *sc) {
  scheme_define(sc, sc->global_env, mk_symbol(sc, "system"), mk_foreign_func(sc, sch_system));
  return 0;
}
