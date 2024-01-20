#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <sys/stat.h>

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


SCM guile_init (void* p) {
    return scm_c_primitive_load(p);
}

SCM guile_update (void*) {
    return scm_call_0(scm_variable_ref(scm_c_lookup("update")));
}

SCM guile_render (void*) {
    return scm_call_0(scm_variable_ref(scm_c_lookup("render")));
}

time_t get_last_modified (const char* path) {
    struct stat st;
    assert(stat(path, &st) == 0, "Failed to stat file: %s\n", path);
    return st.st_mtime;
}

void reset_script(char* script_path) {
    printf("Clearing model data & reloading guile code...\n");
    clear_model_data();
    run_scm(guile_init, script_path);
}


int main (int argc, char** argv) {
    if (argc < 2) {
        printf("Usage: %s path/to/script.scm [--auto-reload]?\n", argv[0]);
        return 1;
    }

    char* script_path = argv[1];
    time_t script_last_modified = get_last_modified(script_path);
    printf("bound script: %s\n", script_path);

    bool enable_auto_reload = false;
    if (argc == 3) {
        char* auto_reload = argv[2];
        if (strcmp(auto_reload, "--auto-reload") == 0) {
            enable_auto_reload = true;
        }
    }

    printf("auto-reload script on file modify: %s\n", enable_auto_reload ? "on" : "off");

    printf(
        "press ctrl+r to reload script\n"
        "press ctrl+w to toggle auto-reload script on file modify\n"
    );

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

    run_scm(guile_init, script_path);

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
                            continue;

                        default:
                            continue;
                    }

                case SDL_KEYUP:
                    if (event.key.keysym.mod & KMOD_CTRL) {
                        switch (event.key.keysym.sym) {
                            case SDLK_r:
                                reset_script(script_path);
                                goto body;

                            case SDLK_w:
                                enable_auto_reload = !enable_auto_reload;
                                printf("auto-reload script on file modify: %s\n", enable_auto_reload ? "on" : "off");
                                continue;

                            default:
                                continue;
                        }
                    } else {
                        continue;
                    }

                default:
                    continue;
            }
        }

        if (enable_auto_reload) {
            time_t new_script_last_modified = get_last_modified(script_path);

            if (difftime(new_script_last_modified, script_last_modified) > 0) {
                script_last_modified = new_script_last_modified;
                reset_script(script_path);
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
