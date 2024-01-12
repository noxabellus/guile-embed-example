#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "glue/gl.h"

#include <SDL2/SDL.h>
#include <SDL2/SDL_opengl.h>

#include <libguile.h>

#define WINDOW_WIDTH 640
#define WINDOW_HEIGHT 480


void* register_functions (void* data) {
  scm_c_define_module("gl", init_glue_gl, data);

  return NULL;
}


SCM handle_exn (void*, SCM key, SCM args) {
  SCM str = scm_symbol_to_string(key);
  SCM str2 = scm_object_to_string(args, SCM_UNDEFINED);

  char* key_s = scm_to_utf8_string(str);
  char* args_s = scm_to_utf8_string(str2);

  printf("'%s exception in script: %s\n", key_s, args_s);

  exit(1);
}

SCM run_scm (scm_t_catch_body body, void* body_data) {
  return scm_c_catch(scm_from_bool(true), body, body_data, NULL, NULL, handle_exn, NULL);
}


SCM guile_init (void*) {
  return scm_c_primitive_load("scripts/hello.scm");
}

SCM guile_render (void*) {
  return scm_call_0(scm_variable_ref(scm_c_lookup("render")));
}

void main_loop(SDL_Window* window) {
  run_scm(guile_render, NULL);

  SDL_GL_SwapWindow(window);
}

int main (int argc, char** argv) {
  scm_with_guile(&register_functions, NULL);

  if (SDL_Init(SDL_INIT_VIDEO) < 0) {
    printf("SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
    return 1;
  }

  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

  SDL_Window* window = SDL_CreateWindow(
      "SDL/OpenGL/Guile",
      SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
      WINDOW_WIDTH, WINDOW_HEIGHT,
      SDL_WINDOW_OPENGL
    );
  if (window == NULL) {
    printf("Window could not be created! SDL_Error: %s\n", SDL_GetError());
    return 1;
  }

  SDL_GLContext gl_context = SDL_GL_CreateContext(window);
  if (gl_context == NULL) {
    printf("OpenGL context could not be created! SDL_Error: %s\n", SDL_GetError());
    return 1;
  }

  glewExperimental = GL_TRUE;
  GLenum glew_res = glewInit();
  if (glew_res != GLEW_OK) {
    printf("Error initializing GLEW! %s\n", glewGetErrorString(glew_res));
    return 1;
  }

  SDL_GL_SetSwapInterval(1); // vsync

  run_scm(guile_init, NULL);
  
  bool should_run = true;
  while (should_run) {
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        if (event.type == SDL_WINDOWEVENT && event.window.event == SDL_WINDOWEVENT_CLOSE) {
            should_run = false;
            break;
        }
    }
    main_loop(window);
  }


  return 0;
}