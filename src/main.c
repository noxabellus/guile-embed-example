#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include "glue/bind.h"

#include <SDL2/SDL.h>
#include <SDL2/SDL_opengl.h>

#include <libguile.h>

#define panic(fmt, ...) (fprintf(stderr, fmt, __VA_ARGS__), exit(1))
#define assert(cond, fmt, ...) if (!(cond)) panic(fmt, __VA_ARGS__)




SCM handle_exn (void*, SCM key, SCM args) {
  SCM str = scm_symbol_to_string(key);
  SCM str2 = scm_object_to_string(args, SCM_UNDEFINED);

  char* key_s = scm_to_utf8_string(str);
  char* args_s = scm_to_utf8_string(str2);

  panic("'%s exception in script: %s\n", key_s, args_s);
}

SCM run_scm (scm_t_catch_body body, void* body_data) {
  return scm_c_catch(scm_from_bool(true), body, body_data, NULL, NULL, handle_exn, NULL);
}


SCM guile_init (void*) {
  return scm_c_primitive_load("scripts/hello.scm");
}

SCM guile_update (void*) {
  return scm_call_0(scm_variable_ref(scm_c_lookup("update")));
}

SCM guile_render (void*) {
  return scm_call_0(scm_variable_ref(scm_c_lookup("render")));
}


int main (int argc, char** argv) {
  assert(SDL_Init(SDL_INIT_VIDEO) == 0, "SDL_Init failed: %s\n", SDL_GetError());

  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

  SDL_Window* window = SDL_CreateWindow(
      "SDL/OpenGL/Guile",
      SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
      640, 480,
      SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE
    );

  assert(window != NULL, "Window could not be created! SDL_Error: %s\n", SDL_GetError());

  SDL_GLContext gl_context = SDL_GL_CreateContext(window);
  assert(gl_context != NULL, "OpenGL context could not be created! SDL_Error: %s\n", SDL_GetError());

  glewExperimental = GL_TRUE;
  GLenum glew_res = glewInit();
  assert(glew_res == GLEW_OK, "Error initializing GLEW! %s\n", glewGetErrorString(glew_res));

  SDL_GL_SetSwapInterval(1); // vsync

  Model model = { window };
  scm_with_guile((void*(*)(void*)) &bind_glue, &model);
  run_scm(guile_init, NULL);

  
  while (true) {
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        if (event.type == SDL_WINDOWEVENT) {
          switch (event.window.event) {
            case SDL_WINDOWEVENT_CLOSE: goto exit;
            case SDL_WINDOWEVENT_RESIZED:
              glViewport(0, 0, event.window.data1, event.window.data2);
              break;
            default: continue;
          }
        }
    }
    
    run_scm(guile_update, NULL);
    run_scm(guile_render, NULL);

    SDL_GL_SwapWindow(window);
  }

  exit:
    SDL_GL_DeleteContext(gl_context);
    SDL_DestroyWindow(window);
    return 0;
}