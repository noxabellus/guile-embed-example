#include <stdint.h>
#include <GL/glew.h>
#include <libguile.h>

#include "glue-gl.h"

char SHADER_INFO_LOG[SHADER_INFO_LOG_LENGTH] = {};

GLuint glMakeShader (GLenum type, const char* source) {
  GLuint shader = glCreateShader(type);
  glShaderSource(shader, 1, &source, NULL);
  glCompileShader(shader);

  GLuint success;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
  if (!success) {
      glGetShaderInfoLog(shader, SHADER_INFO_LOG_LENGTH, NULL, SHADER_INFO_LOG);
      return 0;
  }

  return shader;
}

GLuint glMakeProgram (size_t count, GLuint* shaders) {
  GLuint program = glCreateProgram();
  for (int i = 0; i < count; i++) {
    glAttachShader(program, shaders[i]);
  }
  glLinkProgram(program);

  GLuint success;
  glGetProgramiv(program, GL_LINK_STATUS, &success);
  if (!success) {
      glGetProgramInfoLog(program, SHADER_INFO_LOG_LENGTH, NULL, SHADER_INFO_LOG);
      return 0;
  }

  return program;
}



SCM glue_glClearColor (SCM r, SCM g, SCM b, SCM a) {
  glClearColor(scm_to_double(r), scm_to_double(g), scm_to_double(b), scm_to_double(a));
  return SCM_UNSPECIFIED;
}

SCM glue_glClear (SCM mask) {
  glClear(scm_to_int(mask));
  return SCM_UNSPECIFIED;
}



SCM glue_glMakeShader (SCM type, SCM source) {
  GLuint shader = glMakeShader(scm_to_int(type), scm_to_utf8_string(source));
  if (shader == 0) scm_throw(scm_from_utf8_symbol("shader-compile-failed"), scm_from_utf8_string(SHADER_INFO_LOG));
  else return scm_from_int(shader);
}

SCM glue_glMakeProgram (SCM shaders) {
  size_t count = scm_to_int(scm_length(shaders));
  GLuint shaders_arr[16];
  for (int i = 0; i < count; i++) {
    shaders_arr[i] = scm_to_int(scm_list_ref(shaders, scm_from_int(i)));
  }

  GLuint program = glMakeProgram(count, shaders_arr);
  if (program == 0) scm_throw(scm_from_utf8_symbol("program-link-failed"), scm_from_utf8_string(SHADER_INFO_LOG));
  else return scm_from_int(program);
}

SCM glue_glUseProgram (SCM program) {
  glUseProgram(scm_to_int(program));
  return SCM_UNSPECIFIED;
}



SCM glue_glGenBuffer () {
  GLuint buffer;
  glGenBuffers(1, &buffer);
  return scm_from_int(buffer);
}

SCM glue_glBindBuffer (SCM target, SCM buffer) {
  glBindBuffer(scm_to_int(target), scm_to_int(buffer));
  return SCM_UNSPECIFIED;
}

SCM glue_glBufferData (SCM target, SCM data, SCM usage) {
  scm_t_array_handle handle;
  size_t size;
  size_t len;
  const void* buffer;
  ssize_t inc_p;
  
  if (scm_f32vector_p(data)) {
      size = sizeof(float) * len;
      buffer = scm_f32vector_elements(data, &handle, &len, &inc_p);
  } else if (scm_u32vector_p(data)) {
      size = sizeof(uint32_t) * len;
      buffer = scm_u32vector_elements(data, &handle, &len, &inc_p);
  } else if (scm_s32vector_p(data)) {
      size = sizeof(int32_t) * len;
      buffer = scm_s32vector_elements(data, &handle, &len, &inc_p);
  } else {
      scm_throw(scm_from_utf8_symbol("unsupported-gl-buffer-type"), scm_from_utf8_string("Only f32, u32, and s32 are supported as of now"));
  }

  if (inc_p != 1) scm_throw(scm_from_utf8_symbol("unsupported-gl-buffer-type"), scm_from_utf8_string("Only contiguous buffers are supported as of now"));

  glBufferData(scm_to_int(target), size, buffer, scm_to_int(usage));
  return SCM_UNSPECIFIED;
}



SCM glue_glGenVertexArray () {
  GLuint vao;
  glGenVertexArrays(1, &vao);
  return scm_from_int(vao);
}

SCM glue_glBindVertexArray (SCM vao) {
  glBindVertexArray(scm_to_int(vao));
  return SCM_UNSPECIFIED;
}

SCM glue_glVertexAttribPointer (SCM index, SCM size, SCM type, SCM normalized, SCM stride) {
  glVertexAttribPointer(scm_to_int(index), scm_to_int(size), scm_to_int(type), scm_to_bool(normalized), scm_to_int(stride), NULL);
  return SCM_UNSPECIFIED;
}

SCM glue_glEnableVertexAttribArray (SCM index) {
  glEnableVertexAttribArray(scm_to_int(index));
  return SCM_UNSPECIFIED;
}



SCM glue_glDrawArrays (SCM mode, SCM first, SCM count) {
  glDrawArrays(scm_to_int(mode), scm_to_int(first), scm_to_int(count));
  return SCM_UNSPECIFIED;
}



void init_glue_gl () {
    scm_c_define("gl:+float+", scm_from_int(GL_FLOAT));
    scm_c_define("gl:+unsigned-int+", scm_from_int(GL_UNSIGNED_INT));
    scm_c_define("gl:+int+", scm_from_int(GL_INT));

    scm_c_define("gl:+vertex-shader+", scm_from_int(GL_VERTEX_SHADER));
    scm_c_define("gl:+fragment-shader+", scm_from_int(GL_FRAGMENT_SHADER));

    scm_c_define("gl:+array-buffer+", scm_from_int(GL_ARRAY_BUFFER));
    scm_c_define("gl:+element-array-buffer+", scm_from_int(GL_ELEMENT_ARRAY_BUFFER));

    scm_c_define("gl:+stream-draw+", scm_from_int(GL_STREAM_DRAW));
    scm_c_define("gl:+static-draw+", scm_from_int(GL_STATIC_DRAW));
    scm_c_define("gl:+dynamic-draw+", scm_from_int(GL_DYNAMIC_DRAW));

    scm_c_define("gl:+triangles+", scm_from_int(GL_TRIANGLES));
    
    scm_c_define("gl:+color-buffer-bit+", scm_from_int(GL_COLOR_BUFFER_BIT));
    scm_c_define("gl:+depth-buffer-bit+", scm_from_int(GL_DEPTH_BUFFER_BIT));



    scm_c_define_gsubr("gl:clear-color", 4, 0, 0, glue_glClearColor);
    scm_c_define_gsubr("gl:clear", 1, 0, 0, glue_glClear);
    
    scm_c_define_gsubr("gl:make-shader", 2, 0, 0, glue_glMakeShader);
    scm_c_define_gsubr("gl:make-program", 0, 0, 1, glue_glMakeProgram);
    scm_c_define_gsubr("gl:use-program", 1, 0, 0, glue_glUseProgram);

    scm_c_define_gsubr("gl:gen-buffer", 0, 0, 0, glue_glGenBuffer);
    scm_c_define_gsubr("gl:bind-buffer", 2, 0, 0, glue_glBindBuffer);
    scm_c_define_gsubr("gl:buffer-data", 3, 0, 0, glue_glBufferData);

    scm_c_define_gsubr("gl:gen-vertex-array", 0, 0, 0, glue_glGenVertexArray);
    scm_c_define_gsubr("gl:bind-vertex-array", 1, 0, 0, glue_glBindVertexArray);
    scm_c_define_gsubr("gl:vertex-attrib-pointer", 5, 0, 0, glue_glVertexAttribPointer);
    scm_c_define_gsubr("gl:enable-vertex-attrib-array", 1, 0, 0, glue_glEnableVertexAttribArray);

    scm_c_define_gsubr("gl:draw-arrays", 3, 0, 0, glue_glDrawArrays);
}