#include <libguile.h>

#include "glue/bind.h"

#include "glue/gl.h"
#include "glue/sdl.h"

Model* bind_glue (Model* model) {
  scm_c_define_module("gl", bind_gl_glue, model);
  scm_c_define_module("sdl", bind_sdl_glue, model);

  return model;
}

void clear_model_data () {
  clear_gl_data();
  clear_sdl_data();
}
