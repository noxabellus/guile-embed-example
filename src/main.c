#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include <GL/glew.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_opengl.h>

#include <libguile.h>

#include "glue/bind.h"
#include "util.h"


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

    scm_init_guile();

    Model* model = calloc(1, sizeof(Model));
    model->window = window;

    bind_glue(model);

    run_scm(guile_init, NULL);

    while (true) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
                case SDL_WINDOWEVENT:
                    switch (event.window.event) {
                        case SDL_WINDOWEVENT_CLOSE:
                            goto exit;

                        case SDL_WINDOWEVENT_RESIZED:
                            glViewport(0, 0, event.window.data1, event.window.data2);
                            goto body;

                        default:
                            goto body;
                    }

                case SDL_KEYUP:
                    if (event.key.keysym.sym == SDLK_r && event.key.keysym.mod & KMOD_CTRL) {
                        printf("Clearing model data & reloading guile code...\n");
                        clear_model_data();
                        run_scm(guile_init, NULL);
                    } else goto body;

                default: goto body;
            }
        }

        body: {
            run_scm(guile_update, NULL);
            run_scm(guile_render, NULL);

            SDL_GL_SwapWindow(window);
        }
    }

    exit: {
        SDL_GL_DeleteContext(gl_context);
        SDL_DestroyWindow(window);
        return 0;
    }
}
