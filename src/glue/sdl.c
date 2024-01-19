#include <SDL2/SDL.h>
#include <libguile.h>

#include "glue/glueing-utils.h"
#include "model.h"

static
Model* MODEL = NULL;


SCM glue_SDL_GetWindowSize () {
    int w, h;
    SDL_GetWindowSize(MODEL->window, &w, &h);
    return scm_s32vector(scm_list_2(scm_from_int(w), scm_from_int(h)));
}


void bind_sdl_glue (void* model) {
    MODEL = model;

    EXPORT_PROCEDURE("get-window-size", 0, 0, 0, glue_SDL_GetWindowSize);
}

void clear_sdl_data () { }
